#!/bin/bash
cd data
solve()
{
  local sum=0
  for ent in `ls`; do
    if [[ -d $ent ]]; then
      pushd . > /dev/null
      cd $ent
      solve
      sum=$((`solve | tail -1` + $sum))
      popd > /dev/null
    elif [[ -f $ent ]]; then
      sum=$((`stat -c "%s" $ent` + $sum))
    fi
  done
  echo $sum
}
free_space=$((70000000 - `solve | tail -1`))
needed=$((30000000 - $free_space))
echo $needed
cur=70000000
for x in `solve`; do
  if [[ $x -ge $needed ]]; then
    if [[ $x -le $cur ]]; then
      cur=$x
    fi
  fi
done
echo $cur