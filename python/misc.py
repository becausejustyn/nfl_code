
import torch

def r2(pred: torch.Tensor, targ: torch.Tensor) -> float:
    '''Returns coefficient of determination'''
    sst = (targ-targ.mean())**2.sum()
    sse = (pred-targ)**2.sum()
    return float((sst - sse) / sst)

import numpy as np

def money_line(arg1, arg2, ndigits = 3):

    fav = np.sort([arg1, arg2], axis = None)[0]
    underdog = np.sort([arg1, arg2], axis = None)[1]
    fav_val = fav * -1
    fav_prob = fav_val / (fav_val + underdog)
    return round(fav_prob, ndigits)
  
def logit(p):
  out = np.log(p/(1 - p))
  return(out)


import pyarrow.dataset as ds

dataset = ds.dataset('pbp/parquet', format='parquet')
df = dataset.to_table(columns = ['game_id', 'home_team', 'away_team', 'season_type', 'week', 'game_date']).to_pandas().drop_duplicates(subset = ['game_id']).reset_index(drop = True)

import pandas as pd

schedule = pd.DataFrame()

schedule = schedule.loc[:, ['season', 'week', 'gameday', 'weekday', 'gametime', 
                           'home_team', 'home_score', 'away_team', 'away_score',
                           'home_moneyline', 'away_moneyline',
                           'spread_line', 
                           'total_line',
                           'result', 'total', 
                           'home_rest', 'away_rest',
                           'div_game', 'roof', 'surface'
                           ]].copy()
schedule['home_win_prob'] = schedule.apply(lambda x: money_line2(x['home_moneyline'], x['away_moneyline']), axis=1)
schedule['away_win_prob'] = schedule.apply(lambda x: 1 - (money_line2(x['home_moneyline'], x['away_moneyline'])), axis=1)
schedule['total_hit'] = schedule.apply(lambda x: 1 if x['total'] > x['total_line'] else 0, axis=1)
schedule['home_covered'] = schedule.apply(lambda x: 1 if 
                                          # home fav                               home underdog
                                          (x['spread_line'] > 0 and x['result'] > x['spread_line']) or 
                                          (x['spread_line'] < 0 and x['result'] > x['spread']) 
                                          else 0, axis=1)
schedule['away_covered'] = schedule.apply(lambda x: 1 if 
                                          # away fav                               away underdog
                                          (x['spread_line'] < 0 and x['result'] < x['spread_line']) or 
                                          (x['spread_line'] > 0 and (x['result'] * -1) > x['spread_line'])
                                          else 0, axis=1)

#home_covered = if_else(spread_line > 0 & (result*-1) > spread_line, 1, 0),
#home_covered = if_else(spread_line > 0 & (result*-1) > spread_line, 1, 0),
#away_covered = if_else(spread_line < 0 & (result*-1) < spread_line, 1, 0)
#away_covered = if_else(spread_line < 0 & (result*-1) > spread_line, 1, 0)




import pandas as pd
import numpy as np
import json
import datetime

import nflfastpy as nfl
import matplotlib.pyplot as plt
from matplotlib import plticker

from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegression

pd.set_option('display.max_columns', 20)

data = pd.concat((pd.read_parquet(__, engine='pyarrow') for __ in [f'~/Documents/nfl/data/pbp/parquet/{x}/play_by_play_{x}.parquet' for x in range(2016, 2022)]))

def dynamic_window_ewma(x):
    """
    Calculate rolling exponentially weighted EPA with a dynamic window size
    """
    values = np.zeros(len(x))
    for i, (_, row) in enumerate(x.iterrows()):
        epa = x.epa_shifted[:i+1]
        if row.week > 10:
            values[i] = epa.ewm(min_periods=1, span=row.week).mean().values[-1]
        else:
            values[i] = epa.ewm(min_periods=1, span=10).mean().values[-1]
            
    return pd.Series(values, index=x.index)


# seperate EPA in to rushing offense, rushing defense, passing offense, passing defense for each team
rushing_offense_epa = data.loc[data['rush_attempt'] == 1, :].groupby(['posteam', 'season', 'week'], as_index=False)['epa'].mean()
rushing_defense_epa = data.loc[data['rush_attempt'] == 1, :].groupby(['defteam', 'season', 'week'], as_index=False)['epa'].mean()
passing_offense_epa = data.loc[data['pass_attempt'] == 1, :].groupby(['posteam', 'season', 'week'], as_index=False)['epa'].mean()
passing_defense_epa = data.loc[data['pass_attempt'] == 1, :].groupby(['defteam', 'season', 'week'], as_index=False)['epa'].mean()

# lag EPA one period back
rushing_offense_epa['epa_shifted'] = rushing_offense_epa.groupby('posteam')['epa'].shift()
rushing_defense_epa['epa_shifted'] = rushing_defense_epa.groupby('defteam')['epa'].shift()
passing_offense_epa['epa_shifted'] = passing_offense_epa.groupby('posteam')['epa'].shift()
passing_defense_epa['epa_shifted'] = passing_defense_epa.groupby('defteam')['epa'].shift()

# In each case, calculate EWMA with a static window and dynamic window and assign it as a column 
rushing_offense_epa['ewma'] = rushing_offense_epa.groupby('posteam')['epa_shifted'].transform(lambda x: x.ewm(min_periods=1, span=10).mean())
rushing_offense_epa['ewma_dynamic_window'] = rushing_offense_epa.groupby('posteam').apply(dynamic_window_ewma).values
rushing_defense_epa['ewma'] = rushing_defense_epa.groupby('defteam')['epa_shifted'].transform(lambda x: x.ewm(min_periods=1, span=10).mean())
rushing_defense_epa['ewma_dynamic_window'] = rushing_defense_epa.groupby('defteam').apply(dynamic_window_ewma).values

passing_offense_epa['ewma'] = passing_offense_epa.groupby('posteam')['epa_shifted'].transform(lambda x: x.ewm(min_periods=1, span=10).mean())
passing_offense_epa['ewma_dynamic_window'] = passing_offense_epa.groupby('posteam').apply(dynamic_window_ewma).values
passing_defense_epa['ewma'] = passing_defense_epa.groupby('defteam')['epa_shifted'].transform(lambda x: x.ewm(min_periods=1, span=10).mean())
passing_defense_epa['ewma_dynamic_window'] = passing_defense_epa.groupby('defteam').apply(dynamic_window_ewma).values

#Merge all the data together
offense_epa = rushing_offense_epa.merge(passing_offense_epa, on=['posteam', 'season', 'week'], suffixes=('_rushing', '_passing')).rename(columns={'posteam': 'team'})
defense_epa = rushing_defense_epa.merge(passing_defense_epa, on=['defteam', 'season', 'week'], suffixes=('_rushing', '_passing')).rename(columns={'defteam': 'team'})
epa = offense_epa.merge(defense_epa, on=['team', 'season', 'week'], suffixes=('_offense', '_defense'))

#remove the first season of data
epa = epa.loc[epa['season'] != epa['season'].unique()[0], :].reset_index(drop=True)
#epa = epa.reset_index(drop=True)


tm = epa.loc[epa['team'] == 'GB', :].assign(
    season_week = lambda x: 'w' + x.week.astype(str) + ' (' + x.season.astype(str) + ')'
).set_index('season_week')

fig, ax = plt.subplots()

# this locator puts ticks at regular intervals
loc = plticker.MultipleLocator(base=16) 
ax.xaxis.set_major_locator(loc)
#rotate the x-axis labels a bit
ax.tick_params(axis='x', rotation=75) 

ax.plot(tm['epa_shifted_passing_offense'], lw=1, alpha=0.5)
ax.plot(tm['ewma_dynamic_window_passing_offense'], lw=2)
ax.plot(tm['ewma_passing_offense'], lw=2);
plt.axhline(y=0, color='red', lw=1.5, alpha=0.5)

ax.legend(['Passing EPA', 'EWMA on EPA with dynamic window', 'Static 10 EWMA on EPA'])
ax.set_title('GB Passing EPA per play')
plt.show()

# merge in game result data to come up with our target variable

schedule = data[
    ['season', 'week', 'home_team', 'away_team', 'home_score', 'away_score']
    ].drop_duplicates().reset_index(drop=True).assign(
        home_team_win = lambda x: (x.home_score > x.away_score).astype(int))

df = schedule.merge(
    epa.rename(
        columns={'team': 'home_team'}), 
        on=['home_team', 'season', 'week']).merge(
            epa.rename(
                columns={'team': 'away_team'}), 
                on=['away_team', 'season', 'week'], 
                suffixes=('_home', '_away'))

target = 'home_team_win'
features = [column for column in df.columns if 'ewma' in column and 'dynamic' in column]

df = df.dropna()
X = df.loc[df['season'] != 2020, features].values
y = df.loc[df['season'] != 2020, target].values

clf = LogisticRegression()
clf.fit(X, y)

accuracy_scores = cross_val_score(clf, X, y, cv=10)
log_losses = cross_val_score(clf, X, y, cv=10, scoring='neg_log_loss')

print(f'Model Accuracy: {np.mean(accuracy_scores)}')
print(f'Neg log loss: {np.mean(log_losses)}')

fig, ax = plt.subplots()
feature_names = ['_'.join(feature_name.split('_')[3:]) for feature_name in features]
coef_ = clf.coef_[0]

features_coef_sorted = sorted(zip(feature_names, coef_), key=lambda x:x[-1], reverse=True)

features_sorted = [feature for feature, _ in features_coef_sorted]
coef_sorted = [coef for _, coef in features_coef_sorted]
ax.set_title('Feature importance')
ax.barh(features_sorted, coef_sorted);
plt.show()



df_2020 = df.loc[(df['season'] == 2020)].assign(
    predicted_winner = lambda x: clf.predict(x[features]),
    home_team_win_probability = lambda x: clf.predict_proba(x[features])[:, 1])[['home_team', 'away_team', 'week', 'predicted_winner', 'home_team_win_probability', 'home_team_win']]

df_2020['actual_winner'] = df_2020.apply(lambda x: x.home_team if x.home_team_win else x.away_team, axis=1)
df_2020['predicted_winner'] = df_2020.apply(lambda x: x.home_team if x.predicted_winner == 1 else x.away_team, axis=1)
df_2020['win_probability'] = df_2020.apply(lambda x: x.home_team_win_probability if x.predicted_winner == x.home_team else 1 - x.home_team_win_probability, axis=1)
df_2020['correct_prediction'] = (df_2020['predicted_winner'] == df_2020['actual_winner']).astype(int)

df_2020 = df_2020.drop(columns=['home_team_win_probability', 'home_team_win'])
df_2020.sort_values(by='win_probability', ascending=False).reset_index(drop=True).head(10)



correct = df_2020.loc[df_2020['correct_prediction'] == 1].groupby('week')['correct_prediction'].sum()
num_games = df_2020.groupby('week')['correct_prediction'].size()
results = correct / num_games

results

print(df_2020.loc[df_2020['week'] == results.idxmax()].sort_values(by='win_probability', ascending=False))

df_2020.loc[df_2020['week'] > 17]


### super bowl prediction

import itertools

def ewma(data, window):
    """
    Calculate the most recent value for EWMA given an array of data and a window size
    """
    alpha = 2 / (window + 1.0)
    alpha_rev = 1 - alpha
    scale = 1 / alpha_rev
    n = data.shape[0]
    r = np.arange(n)
    scale_arr = scale**r
    offset = data[0] * alpha_rev**(r+1)
    pw0 = alpha * alpha_rev**(n-1)
    mult = data * pw0 * scale_arr
    cumsums = mult.cumsum()
    out = offset + cumsums * scale_arr[::-1]
    return out[-1]

data_2020 = data.loc[(data['season'] == 2020)]
offense = data_2020.loc[(data_2020['posteam'] == 'KC') | (data_2020['posteam'] == 'TB')]
defense = data_2020.loc[(data_2020['defteam'] == 'KC') | (data_2020['defteam'] == 'TB')]

rushing_offense = offense.loc[offense['rush_attempt'] == 1].groupby(['posteam', 'week'], as_index=False)['epa'].mean().rename(columns={'posteam': 'team'})
passing_offense = offense.loc[offense['pass_attempt'] == 1].groupby(['posteam', 'week'], as_index=False)['epa'].mean().rename(columns={'posteam': 'team'})
rushing_defense = defense.loc[defense['rush_attempt'] == 1].groupby(['defteam', 'week'], as_index=False)['epa'].mean().rename(columns={'defteam': 'team'})
passing_defense = defense.loc[defense['pass_attempt'] == 1].groupby(['defteam', 'week'], as_index=False)['epa'].mean().rename(columns={'defteam': 'team'})

super_bowl_X = np.zeros(8)

for i, (tm, stat_df) in enumerate(itertools.product(['KC', 'TB'], [rushing_offense, passing_offense, rushing_defense, passing_defense])):
    ewma_value = ewma(stat_df.loc[stat_df['team'] == tm]['epa'].values, 20)
    super_bowl_X[i] = ewma_value

predicted_winner = clf.predict(super_bowl_X.reshape(1, 8))[0]
predicted_proba = clf.predict_proba(super_bowl_X.reshape(1, 8))[0]

winner = 'KC' if predicted_winner else 'TB'
win_prob = predicted_proba[-1] if predicted_winner else predicted_proba[0]

print(f'Model predicts {winner} will win the Super Bowl and has a {round(win_prob*100, 2)}% win probability')
