def max_of(a):
    m = a[0]
    for v in a:
        m = max(v, m)
    return m
def sum_list(a):
    acc = 0
    for v in a:
        acc += int(v)
    return acc
with open('input') as f:
    print(max_of([ sum_list(x.split('\n')) for x in f.read().strip().split('\n\n')]))
