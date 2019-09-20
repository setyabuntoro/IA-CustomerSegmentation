"""
Sample Materials, provided under license.
Licensed Materials - Property of IBM
© Copyright IBM Corp. 2019. All Rights Reserved.
US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
"""
import pandas as pd
import numpy as np
from sklearn import model_selection
from sklearn.externals import joblib
import sys
import os, json
from collections import OrderedDict
from customer_segmentation_prep import CustomerSegmentationPrep

global model
global project_path
global meta_data
model = None
project_path = None
meta_data = None

def init():
    global model
    global project_path
    global meta_data

    # set project_path
    project_path = os.environ.get("DSX_PROJECT_DIR")

    model_name = "KMeans_model"
    version = "latest"
    model_parent_path = project_path + "/models/" + model_name + "/"
    metadata_path = model_parent_path + "metadata.json"

    # fetch info from metadata.json
    with open(metadata_path) as data_file:
        meta_data = json.load(data_file)


    # if latest version, find latest version from  metadata.json
    if (version == "latest"):
        version = meta_data.get("latestModelVersion")

    # prepare model path using model name and version
    model_path = model_parent_path + str(version) + "/model"

    # load model
    model = joblib.load(open(model_path, 'rb'))

def get_prepped_score_data(input_df, sc_end_date, user_inputs):
  
    # globals().update(user_inputs)
    # call the script to prep the data
    scoring_prep = CustomerSegmentationPrep('score', granularity_key=user_inputs['granularity_key'],
                                            customer_start_date=user_inputs['customer_start_date'],
                                            customer_end_date=user_inputs['customer_end_date'],
                                            status_attribute=user_inputs['status_attribute'],
                                            status_flag_active=user_inputs['status_flag_active'],
                                            date_customer_joined=user_inputs['date_customer_joined'],
                                            columns_required=user_inputs['columns_required'],
                                            default_attributes=user_inputs['default_attributes'],
                                            risk_tolerance_list=user_inputs['risk_tolerance_list'],
                                            investment_objective_list=user_inputs['investment_objective_list'],
                                            effective_date=sc_end_date,
                                            std_multiplier=user_inputs['std_multiplier'],
                                            max_num_cat_cardinality=user_inputs['max_num_cat_cardinality'],
                                            nulls_threshold=user_inputs['nulls_threshold'])
    prepped_data = scoring_prep.prep_data(input_df, 'score')
    
    # take a copy of the dataframe before we carry out transformations
    prepped_data_pre_transform = prepped_data.copy()

    # the dataset contains a mix of continuous variables and categorical variables
    # categorical variables are converted into dummy variables
    # continuous variables are standardised using z-score

    numeric_cols = list(prepped_data.select_dtypes(include=[np.number]).columns)
    categorical_cols = list(prepped_data.select_dtypes(include=[object]).columns)

    # create dummy variables for categorical variables and drop original
    for col in categorical_cols:
        prepped_data = pd.concat([prepped_data, pd.get_dummies(prepped_data[col], prefix=col, drop_first=True)], axis=1)
        prepped_data.drop(col, axis=1, inplace=True)
        
    # since we're using distance based clustering, we need to scale numeric variables
    # z score was used in training. Load the mean and std of numeric columns used for training to standardise the scoring data
    try:
        with open(project_path + '/datasets/col_means.json', 'rb') as f:
            dict_col_means = json.load(f)
        with open(project_path + '/datasets/col_stds.json', 'rb') as f:
            dict_col_stds = json.load(f)
    except FileNotFoundError:
        print('File not found. Please rerun the training notebook')

    # loop through each value in dict and standardise the variables
    for col in list(dict_col_means):
        col_mean = dict_col_means[col]
        col_std = dict_col_stds[col]
        prepped_data[col+'_standardised'] = (prepped_data[col] - col_mean)/col_std
        # drop the non-standardised column
        prepped_data.drop(col, axis=1, inplace=True)

    # check to make sure we have all of the same columns and order
    # use the model meta data
    cols_used_for_training = []
    for feature in meta_data['features']:
        cols_used_for_training.append(feature['name'])
        
    # if a column does not exist in scoring but is in training, add the column to scoring dataset
    for col in cols_used_for_training:
        if col not in list(prepped_data.columns):
            prepped_data[col] = 0

    # if a column exists in scoring but not in training, delete it from scoring dataset
    for col in list(prepped_data.columns):
        if col not in cols_used_for_training:
            prepped_data.drop(col, axis=1, inplace=True)

    # make sure order of scoring columns is same as training dataset
    prepped_data = prepped_data[cols_used_for_training]
  
    # get the pca object used in training and apply to the prepped dataset
    pca = joblib.load(project_path + '/datasets/pca.joblib')
    prepped_data = pd.DataFrame(pca.transform(prepped_data))
    print('Transforms complete')
    
    return prepped_data, prepped_data_pre_transform, numeric_cols

def score(args):
    global model
    global project_path
    global meta_data

    # parse input arguments
    dataset_name = args.get("dataset_name")
    cust_ids = args.get("cust_ids")
    sc_end_date = args.get("sc_end_date")

    # load the dataset
    dataset_path = project_path + "/datasets/" + dataset_name
    input_df = pd.read_csv(dataset_path, infer_datetime_format=True,
                        parse_dates=['CUSTOMER_RELATIONSHIP_START_DATE', 'CUSTOMER_SUMMARY_END_DATE', 'CUSTOMER_SUMMARY_START_DATE'])

    input_df = input_df[input_df['CUSTOMER_CUSTOMER_ID'].isin(cust_ids)]
    
    # the customer_segmentation_prep.py scripts saves out the last user inputs used for prepping the data
    # import this dictionary and pass the variables to the prep function
    # this ensures that the inputs used for prepping the training data are the same as those used for prepping the scoring data
    user_inputs_dict = joblib.load(open(project_path + '/datasets/training_user_inputs.joblib', 'rb'))
    
    # call the function to prep the data. This calls the script to prep the raw data but also standardises and dummies the vars
    # as well as transforming the vars using pca (pca object from training)
    # a bit hacky but since we want the original prepped data (before transformations), the function returns that dataframe
    # take the numeric columns too as they will be used in dict with numerical column ranges
    prepped_data, prepped_data_pre_transform, numeric_cols = get_prepped_score_data(input_df, sc_end_date, user_inputs_dict)
    
    # convert to list as json can't take arrays
    predictions = model.predict(prepped_data)
    
    # import the dict with range of values for numerical dataframe
    summary_dict = joblib.load(project_path + '/datasets/numeric_data_ranges.joblib')
    
    # get the keys (cluster names) and filter to only the clusters that are being returned by the scoring
    keys = list(summary_dict.keys())
    filtered_summary_dict = dict((k, summary_dict[k]) for k in keys if k in list(predictions))
    
    for cluster in list(set(predictions)):
      for col in numeric_cols:
          # makes sure we exclude columns like customer id
          if col in list(filtered_summary_dict[cluster]): 
            filtered_summary_dict[cluster][col]['min'] = int(filtered_summary_dict[cluster][col]['min'])
            filtered_summary_dict[cluster][col]['max'] = int(filtered_summary_dict[cluster][col]['max'])
    
    # convert to python ints so that it's JSON serializable
    return_dict = {'assigments':[int(label) for label in predictions], 'features':filtered_summary_dict}
    
    return return_dict

def test_score(args):
    """Call this method to score in development."""
    init()
    return score(args)
