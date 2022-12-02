import numpy as np
parse_elf = lambda x: list(map(int, x.split('\n')))
parse_input = lambda x: [ parse_elf(v) for v in x.strip().split('\n\n') ]
sum_list = lambda x: np.sum(x)
with open('input') as f:
    data = parse_input(f.read())
    totals = list(map(sum_list, data))
    print(np.sum(np.sort(totals)[-3:]))
