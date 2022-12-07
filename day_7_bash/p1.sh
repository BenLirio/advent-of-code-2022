#!/bin/bash

pushd . > /dev/null
while read p; do
  if [[ $p =~ (\$ cd) ]]; then
    cd_cmd=$(echo $p | grep -oP '(?<=\$ )cd .*')
    mkdir_cmd=$(echo $cd_cmd | sed 's/cd/mkdir/g')
    dir=$(echo $cd_cmd | grep -oP '(?<=cd ).*')
    [[ ! -d $dir ]] && $mkdir_cmd
    $cd_cmd
  elif [[ $p =~ (\$ ls) ]]; then
    : # do nothing
  elif [[ $p =~ ([0-9]+ .*) ]]; then
    size=$(echo $p | grep -oP '^[0-9]+')
    fname=$(echo $p | sed s/[0-9]*//g)
    touch $fname
    truncate -s $size $fname
  elif [[ $p =~ (dir .*) ]]; then
    dir=$(echo $p | grep -oP '(?<=dir ).*')
    [[ ! -d $dir ]] && mkdir $dir
  fi
done < input.txt
popd > /dev/null

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
total=0
for x in `solve`; do
  if [[ $x -le 100000 ]]; then
    total=$(($x + $total))
  fi
done
echo $total