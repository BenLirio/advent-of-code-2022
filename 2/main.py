import numpy as np
def parse_input(filename):
  parse_game = lambda game: np.array(game.split(' '))
  with open(filename) as f:
    raw_data = f.read().strip()
    unparsed_games = raw_data.split('\n')
    games = list(map(parse_game, unparsed_games))
    return np.array(games)

def dec_games(k, games):
  out = np.copy(games)
  for key in k:
    val = k[key]
    out[out[:,1]==key,1] = val
  return out

def use_ints(games):
  out = np.copy(games)
  out[out[:,0]=='A', 0] = 0
  out[out[:,0]=='B', 0] = 1
  out[out[:,0]=='C', 0] = 2
  out[out[:,1]=='A', 1] = 0
  out[out[:,1]=='B', 1] = 1
  out[out[:,1]=='C', 1] = 2
  return np.array(out, dtype=int)



with open('input.txt') as f:
  games_enc = parse_input('input.txt')
  k = {
    'X': 'A',
    'Y': 'B',
    'Z': 'C'
  }
  games = dec_games(k, games_enc)
  games_ints = use_ints(games)
  score = 0
  for game in games_ints:
    score += 1
    score += game[1]
    if game[0] == game[1]:
      score += 3
    if ((game[0]+1) % 3) - game[1] == 0:
      score += 6
  print(score)
  score = 0



  for game in games:
    if game[1] == 'A':
      score += 1
    if game[1] == 'B':
      score += 2
    if game[1] == 'C':
      score += 3
    # win
    if (game[0] == 'A' and game[1] == 'B') \
        or (game[0] == 'B' and game[1] == 'C') \
        or (game[0] == 'C' and game[1] == 'A'):
      score += 6
    # tie
    elif game[0] == game[1]:
      score += 3
    # lose
    else:
      pass
  print(score)


