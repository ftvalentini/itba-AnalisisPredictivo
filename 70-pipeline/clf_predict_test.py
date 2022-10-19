
import pickle
import logging

import pandas as pd

logging.basicConfig(
    level=logging.INFO, format='%(asctime)s [%(levelname)s] %(message)s')

logging.info('Loading model...')
with open("best_pipe.pkl", "rb") as f:
    pipe = pickle.load(f)

logging.info('Loading test data...')
df_test = pd.read_csv("data_test.csv", index_col=0)

logging.info('Predicting...')
X_test = df_test.drop(["target"], axis=1)
y_pred = pipe.predict_proba(X_test)[:, 1]

logging.info('Saving predictions...')
df_out = pd.DataFrame({"id": X_test.index, "y_pred": y_pred})
df_out.sort_values("id", inplace=True)
print(df_out.head())
# df_out.to_csv("predictions.csv", index=False)

