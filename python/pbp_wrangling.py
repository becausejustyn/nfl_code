
import pandas as pd
import numpy as np

pd.set_option('display.max_columns', 40)

pbp = pd.concat((pd.read_parquet(__, engine='pyarrow') for __ in [f'~/Downloads/nfl_pbp/play_by_play_{x}.parquet' for x in range(1999, 2022)]))

# for some reason 2000_11_OAK_DEN has wrong values
pbp = pbp[(pbp.season_type == 'REG') & (pbp.game_id != '2000_11_OAK_DEN')][
    ['game_id', 'game_date', 'season', 'week', 'home_team', 'away_team', 'home_score', 'away_score', 'spread_line', 'total', 'total_line', 'result']
    ].groupby('game_id').last().reset_index()

pbp1 = pbp.assign(
    home_win = lambda x: np.where(x.home_score > x.away_score, 1, 0),
    home_fav = lambda x: np.where(x.spread_line > 0, 1, 0),
    away_fav = lambda x: np.where(x.spread_line < 0, 1, 0),
    home_cover = lambda x: np.where(
        x.home_fav == 1,
        ((x.home_score - x.spread_line) > x.away_score), # (x.home_score > (x.spread_line + x.away_score))
        ((x.away_score + x.spread_line *-1) < x.home_score)),
    away_cov = lambda x: np.where(
        x.away_fav == 1,
        ((x.away_score + x.spread_line) > x.home_score),
        ((x.away_score - x.spread_line *-1) > x.home_score))
)