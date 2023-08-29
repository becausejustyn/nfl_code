
import numpy as np


def money_line(arg1, arg2, ndigits = 3):
    fav = np.sort([arg1, arg2], axis = None)[0]
    underdog = np.sort([arg1, arg2], axis = None)[1]
    fav_val = fav * -1
    fav_prob = fav_val / (fav_val + underdog)
    return round(fav_prob, ndigits)

def calc_expected_score(team_rating, opp_team_rating):
    return 1 / (1 + 10**((opp_team_rating - team_rating) / 400))


def calc_expected_score2(team_rating, opp_team_rating, team_pts, opp_pts):
    '''With margin of victory multiplier'''
    elo_diff = np.abs(team_rating - opp_team_rating)
    winner_pt_diff = np.abs(team_pts - opp_pts)
    return np.log(winner_pt_diff + 1) * (2.2 / ((elo_diff) * .001 + 2.2))

def calc_new_rating(team_rating, observed_score, expected_score, k_factor = 20):
    return team_rating + k_factor * (observed_score - expected_score)

def money_line_odds(a, b):
    def calculate_odds(aa):
        odds = np.where(aa < 0, ((-aa) / (-aa + 100)), (100 / (aa + 100)))
        return odds
    odds_a = calculate_odds(a)
    odds_b = calculate_odds(b)
    implied_a = odds_a / (odds_a + odds_b)
    implied_b = odds_b / (odds_a + odds_b)
    return implied_a, implied_b


def getOdds(a):
    """getOdds finds the breakeven probability, given American odds 'a'"""

    odds = 0
    if a < 0:
        odds = (-a)/(-a + 100)
    else:
        odds = 100/(100+a)

    return odds

def impliedOdds(a, b):
    """Given American odds a and b of some two-outcome event, impliedOdds finds
    the implied odds of each event occurring."""

    oddsA = getOdds(a)
    oddsB = getOdds(b)

    impliedA = oddsA/(oddsA + oddsB)
    impliedB = oddsB/(oddsA + oddsB)

    print("Team A has an implied Vegas win probability of " + str(impliedA))
    print("Team B has an implied Vegas win probability of " + str(impliedB))

def moneylineToProb(moneyline):
    if moneyline > 0:
        prob = moneyline / (moneyline + 100)
        return round(prob * 100, 2)
    else:
        prob = (moneyline * -1) / ((moneyline * -1) + 100)
        return round(prob * 100, 2)

def moneyline_prob_converter(num):
    if num > 0:
        return 100 / (num + 100)
    else:
        return (-1 * (num)) / ((-1 * num) + 100)