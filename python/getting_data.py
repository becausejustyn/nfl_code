
import pandas as pd
import numpy as np
import glob

all_files = glob.glob('/Users/justynrodrigues/Documents/nfl/data/pbp/csv/*.csv.gz')
df = pd.concat((pd.read_csv(__, low_memory=False, index_col=0) for __ in all_files))

df = (df
.loc[(df['season_type'] == 'REG') & (df['qtr'] <= 4) & (df['posteam_type'].notnull()), :].reset_index()[
    ['game_id', 'season', 'week', 'posteam_type', 'season_type', 'home_team', 'away_team', 'home_score', 'away_score']]
    .drop_duplicates('game_id')
    .query('home_score != away_score')
    .assign(
        home_result = lambda x: x['home_score'] - x['away_score'],
        home_win = lambda x: np.where(x['home_result'] > 0, 1, 0)
        ))

df.to_csv('/Users/justynrodrigues/Documents/Github/nfl_hfa/python/df.csv.gz', compression='gzip')