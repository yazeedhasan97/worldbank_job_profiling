import glob

import joblib
import pandas as pd
from django.conf import settings
import os
import numpy as np

# Attributes and contributors
GOVERNORATE = 'governorate'
AGE = 'age'
EXPERIENCE = 'experience'
EDUCATION = 'education'
GENDER = 'gender'
DISABILITY = 'disability'

# Integrity strings
NA_FILL_VALUE = 'NA_FILL_VALUE'
CATEGORIES = 'CATEGORIES'
CODE = 'CODE'

API_CONTRIBUTORS = [GOVERNORATE, AGE, EXPERIENCE, EDUCATION, GENDER, DISABILITY]
API_ONEHOT_ENCODED = [GOVERNORATE, EDUCATION,]
API_LABEL_ENCODED = [GENDER, DISABILITY]

API_ENCODERS_OUTPUT_ITEMS = [
    'experience', 'age', 'disability',
    'governorate_ajloun', 'governorate_al_aqaba', 'governorate_al_kirk', 'governorate_al_mafraq',
    'governorate_amman', 'governorate_balqa', 'governorate_irbid', 'governorate_jarash', 'governorate_maadaba',
    'governorate_maan', 'governorate_outside_jordan', 'governorate_tafileh', 'governorate_zarqa', 'education_bachelor_or_above',
    'education_middle_diploma', 'education_secondary_or_below', 'education_vocational_training'
]



API_ENCODERS = {}


def disability_code(value):
    if value is None:
        return None

    value = str(value).lower()
    if value not in ['no_disability', 'nodisability', 'no', 'not_disabled']:
        return 'with_disability'
    return 'no_disability'


def experience_code(value):
    if value is None:
        return None

    value = float(value)
    if value > 15:
        return 20
    elif value > 10:
        return 15
    elif value > 5:
        return 10
    elif value > 1:
        return 5
    elif value > 0:
        return 1
    else:
        return 0


def age_code(value):
    if value is None:
        return None

    age = int(round(float(value) / 10) * 10)
    if age > 60:
        return 60
    elif age < 20:
        return 20
    else:
        return age


def education_code(value):
    if value is None:
        return None

    value = str(value).lower()
    education_subs = {
        'bachelor_or_above': [
            'bachelor_or_above', 'bachelor', 'bachelors', "bs", "b.s", "bas",
            'master', 'masters', 'm.s', 'ms', 'phd', 'doctor_of_philosophy', 'doctorate', 'doctorates'
        ],
        'vocational_training': [
            "vocational_training", 'vt', 'v.t'
        ],
        'middle_diploma': [
            "middle_diploma", 'diploma' "high_diploma", 'deploma', "high_deploma", "middle_deploma",
        ],
        'secondary_or_below': [
            "secondary_or_below", 'high_school', 'school', 'secondary', 'secondary_school'
        ],
    }
    for key in education_subs:
        if value in education_subs[key]:
            return key

    return TEMPLATE_DATA_CATEGORIES[EDUCATION][NA_FILL_VALUE]


def governorate_code(value):
    if value is None:
        return None

    value = str(value).lower()  # .replace(f'governorate_', '')
    governorate_subs = {
        'al_kirk': ['al_kark', 'al_kirk', 'kark', 'kirk', ],
        'balqa': ['balqa', 'al_balqa', 'balqaa', 'al_balqaa', ],
        'tafileh': ['tafileh', 'al_tafileh', ],
        'jarash': ['jarash', 'jerash'],
        'zarqa': ['zarqa', 'al_zarqa'],
        'amman': ['amman', 'aman'],
        'al_mafraq': ['al_mafraq', 'mafraq'],
        'maan': ['maan', ],
        'irbid': ['irbid', 'irbed'],
        'al_aqaba': ['al_aqaba', 'aqaba'],
        'maadaba': ['maadaba', 'madaba'],
        'ajloun': ['ajloun'],
    }
    for key in governorate_subs:
        if value in governorate_subs[key]:
            return key
    return TEMPLATE_DATA_CATEGORIES[GOVERNORATE][NA_FILL_VALUE]


def gender_code(value):
    if value is None:
        return None

    value = str(value).lower()
    gender_subs = {
        'male': ['male', 'm', 'man'],
        'female': ['female', 'f', 'woman'],
    }
    for key in gender_subs:
        if value in gender_subs[key]:
            return key
    return TEMPLATE_DATA_CATEGORIES[GENDER][NA_FILL_VALUE]


TEMPLATE_DATA_CATEGORIES = {
    DISABILITY: {
        CATEGORIES: ['with_disability', 'no_disability'],
        # CODE: "utils.disability_code({0})",
        CODE: "disability_code({0})",
        NA_FILL_VALUE: 'no_disability'
    },
    EXPERIENCE: {
        CATEGORIES: [0, 1, 5, 10, 15, 20],
        # CODE: "utils.experience_code({0})",
        CODE: "experience_code({0})",
        NA_FILL_VALUE: 0
    },
    AGE: {
        CATEGORIES: [10, 20, 30, 40, 50, 60],
        # CODE: "utils.age_code({0})",
        CODE: "age_code({0})",
        NA_FILL_VALUE: 30
    },

    EDUCATION: {
        CATEGORIES: ['bachelor_or_above', 'vocational_training', 'middle_diploma', 'secondary_or_below'],
        # CODE: "utils.education_code({0})",
        CODE: "education_code({0})",
        NA_FILL_VALUE: 'secondary_or_below'
    },
    GOVERNORATE: {
        CATEGORIES: [
            'ajloun', 'al_aqaba', 'al_kirk',
            'al_mafraq', 'amman', 'balqa',
            'irbid', 'jarash', 'maadaba',
            'maan', 'tafileh', 'zarqa'
        ],
        # CODE: "utils.governorate_code({0})",
        CODE: "governorate_code({0})",
        NA_FILL_VALUE: 'amman'
    },
    GENDER: {
        CATEGORIES: ['male', 'female'],
        # CODE: "utils.gender_code({0})",
        CODE: "gender_code({0})",
        NA_FILL_VALUE: 'male'
    },

}


def preprocess_recs(rec, cols_lst):
    dct = {}
    for column in cols_lst:
        value = rec.get(column, None)
        res = eval(TEMPLATE_DATA_CATEGORIES[column][CODE].format('value'))
        dct[column] = res

    print('After preprocessing:', dct)
    return dct


def load_encoders(into=None, path=None):
    into = into.copy() if into is not None else {}

    if path is None:
        path = os.path.join(settings.MODELS_PATH, f'*_encoders.joblib')

    for enc in glob.iglob(path):
        name = enc.split('\\')[-1].split('_')[0]
        into[name] = joblib.load(enc)

    print("Loaded encoders", into)
    return into


def encode_new_data(data, cols_lst, encoders, onehot_encoded, label_encoded):
    model_input = {}
    for feature in cols_lst:
        value = data.get(feature, None)
        # print(value)
        if feature in label_encoded:
            model_input[feature] = int(encoders[feature].transform([value])[0]) if value is not None else value

        elif feature in onehot_encoded:
            if value is not None:
                item = encoders[feature].transform([[value]])[-1]
                lst = {name: int(value) for name, value in zip(encoders[feature].get_feature_names_out(), item)}
            else:
                lst = {
                    i: None for i in encoders[feature].get_feature_names_out()
                }
            model_input.update(lst)

        else:
            model_input[feature] = int(value) if value is not None else value
    return model_input


def reorder_cols(data_dct, order):
    res = pd.DataFrame(data_dct, index=[0])[order]
    if data_dct[GENDER] == 0 or data_dct[GENDER] == 'female':
        res.drop('governorate_outside_jordan', axis=1, inplace=True)
    return res, data_dct[GENDER]


def follow_the_crowd_for_missing_value(model,
                                       data):  # this is a brute force solution. I beleive there is a better one can be found
    """This function build the tree structure then select a node for it iff it has missing values """
    n_nodes = model.tree_.node_count
    children_left = model.tree_.children_left
    children_right = model.tree_.children_right
    feature = model.tree_.feature
    name = model.feature_names_in_
    threshold = model.tree_.threshold
    samples = model.tree_.n_node_samples

    node_depth = np.zeros(shape=n_nodes, dtype=np.int64)
    is_leaves = np.zeros(shape=n_nodes, dtype=bool)
    stack = [(0, 0)]  # start with the root node id (0) and its depth (0)
    while len(stack) > 0:
        # `pop` ensures each node is only visited once
        node_id, depth = stack.pop()
        node_depth[node_id] = depth

        # If the left and right child of a node is not the same we have a split
        # node
        is_split_node = children_left[node_id] != children_right[node_id]
        # If a split node, append left and right children and depth to `stack`
        # so we can loop through them
        if is_split_node:
            stack.append((children_left[node_id], depth + 1))
            stack.append((children_right[node_id], depth + 1))
        else:
            is_leaves[node_id] = True

    # print( "The binary tree structure has {n} nodes, {l} leaves and has the following tree structure:\n".format(n=n_nodes, l=np.sum(is_leaves)))
    current_node = 0
    for i in range(n_nodes):
        # print('current_node is', current_node, end=' ::: ')
        if is_leaves[i]:
            # print("{space}node={node} is a leaf node.".format(space=node_depth[i] * "\t", node=i))
            pass
        else:
            # print(f"Check for {name[feature[i]].upper()}")
            if data[name[feature[i]]].values in [np.nan, None]:
                if current_node == i:
                    # print(f"{node_depth[i] * '    '}node={i} is a split node for None values: go to node {children_left[i]} if SAMPLES(node {children_left[i]}) {samples[children_left[i]]} >= SAMPLES(node {children_right[i]}) {samples[children_right[i]]} else to node {children_right[i]}.")
                    if samples[children_left[i]] >= samples[children_right[i]]:
                        current_node = children_left[i]
                    else:
                        current_node = children_right[i]
            else:
                if current_node == i:
                    # print(f"{node_depth[i] * '    '}node={i} is a split node: go to node {children_left[i]} if X[:, {feature[i]} {name[feature[i]]}] <= {threshold[i]} else to node {children_right[i]}.")
                    if data[name[feature[i]]].values <= threshold[i]:
                        current_node = children_left[i]
                    else:
                        current_node = children_right[i]

        if is_leaves[current_node]:
            # print(f'Node {current_node} is a leaf node. Its the node where this person will land')
            # return current_node, np.argmax(model.tree_.value[current_node])
            return current_node, samples[current_node]

    # return current_node, np.argmax(model.tree_.value[current_node])
    return current_node, samples[current_node]


def surv_tree_predict_node(gender, data=None):
    print('Loading Model:', os.path.join(settings.MODELS_PATH, f'{gender}_surv_tree_model.joblib'))
    surv_tree = joblib.load(os.path.join(settings.MODELS_PATH, f'{gender}_surv_tree_model.joblib'))
    # node = np.round(surv_tree.predict(data), 8).astype(object)

    print('Loading Mapper:', os.path.join(settings.MODELS_PATH, f'{gender}_cluster_node_dict_mapper.joblib'))
    node_cluster_mapper = joblib.load(os.path.join(settings.MODELS_PATH, f'{gender}_cluster_node_dict_mapper.joblib'))

    print('Loading Mapper Corrector:', os.path.join(settings.MODELS_PATH, f'{gender}_cluster_node_dict_cmapper.joblib'))
    cluster_corrector_mapper = joblib.load(
        os.path.join(settings.MODELS_PATH, f'{gender}_cluster_node_dict_cmapper.joblib'))

    if None in data.values or np.nan in data.values:
        print('This rec has missing.')
        node, samples_c = follow_the_crowd_for_missing_value(surv_tree, data)
        print(f"Follow the crowd lead to node -> {node}, with samples of {samples_c}")
        cluster = node_cluster_mapper[node]
        print(f"Node to Cluster Mapping lead to -> {cluster}")
    else:
        print('This rec is complete.')
        node = surv_tree.tree_.apply(data.to_numpy().astype('float32'))
        print(f"Using the SurvTree prediction lead to node -> {node}")
        cluster = node_cluster_mapper[node[-1]]
        print(f"Node to Cluster Mapping lead to -> {cluster}")

    # if list(cluster_corrector_mapper.keys()) != list(cluster_corrector_mapper.values()):
    cluster = cluster_corrector_mapper[cluster]
    print(f"Correcting the Cluster lead to -> {cluster}")

    return cluster, node


def predict(data_dct, contributers, encoders, onehot, labeled, encoders_outputs_feature, ):
    data = data_dct.copy()
    data = preprocess_recs(
        data,
        contributers
    )

    data = encode_new_data(data, contributers, encoders, onehot, labeled)

    data, gen = reorder_cols(data, encoders_outputs_feature)
    # display(data)

    return surv_tree_predict_node(
        gender='male' if gen == 1 else 'female',
        data=data
    )

