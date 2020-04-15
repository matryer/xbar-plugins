#!/bin/bash

# <bitbar.title>Rigged Coin Flip</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tyllis Xu</bitbar.author>
# <bitbar.author.github>livelycarpet87</bitbar.author.github>
# <bitbar.image>https://i.ibb.co/YjYbHms/Rigged-Coin-Flip.png</bitbar.image>
# <bitbar.desc>A elaborately rigged coin flip. It can be set to varying degrees of unfairness and results (and its fairness) can be modified by the option key. </bitbar.desc>

#Configuration START

#Fairness
# 2: extreme realism
# 1: fair
# 0: Unfair
fair=0
#Probability for favored outcome
#This does not influence fair flips
# 50 to 100
prob=90
#favored
# {H=favor heads|T=favor tails}
favored=T
#Option Key Effect
# 1: Reverse Favored Outcome
# 2: Reverse Fairness
# 3: Print Config (NOT RECOMMENDED)
opt=2

#Configuration END

#Start Functions

fairFlip(){
  ((i=RANDOM % 100 + 1))
  if [[ $i -le 50 ]]
  then
    echo "H"
  else
    echo "T"
  fi
}
unfairFlip(){
  ((i=RANDOM % 100 + 1))
  if [[ $i -le $prob ]]
  then
    echo "$favored"
  elif [[ $i -le 100 ]]
 then
    echo "$favored"
  else
    echo "Invalid Configuration!"
  fi
}
realFlip(){
  ((i=RANDOM % 6000 + 1))
  if [[ $i == 1 ]]
  then
    echo "The coin lands on its side and is perfectly still..."
  elif [[ $i -le 2940 ]]
  then
    echo "$unfavored"
  else
    echo "$favored"
  fi
}

alternative(){
if [[ $opt == 1 ]]
then
  ((i=RANDOM % 100 + 1))
  if [[ $i -le $prob ]]
  then
    echo "$unfavored"
  elif [[ $i -le 100 ]]
 then
    echo "$favored"
  else
    echo "Invalid Configuration!"
  fi
elif [[ $opt == 2 ]]
then
  if [[ $fair == 1 || $fair == 2 ]]
  then
    unfairFlip
  elif [[ $fair == 0 ]]
  then
    fairFlip
  else
    echo "Invalid Configuration!"
  fi
elif [[ $opt == 3 ]]
then
  echo "Fairness is $fair. Probability of favored outcome is $prob. Favored outcome is $favored. Option key setting is NO. $opt. Enjoy. "
else
    echo "Invalid Configuration!"
fi
}

#End Functions

#Initialize variables
if [[ $favored == H ]]
then
  unfavored=T
elif [[ $favored == T ]]
then
  unfavored=H
else
  echo "Invalid Configuration!"
  exit
fi


echo " :question: |dropdown=false"
echo " :grey_question: |dropdown=false"
echo "---"
echo Result
if [[ $fair == 0 ]]
then
  echo "-- $(unfairFlip)"
  echo "-- $(alternative) | alternate=true"

elif [[ $fair == 1 ]]
then
  echo "-- $(fairFlip)"
  echo "-- $(alternative) | alternate=true"
elif [[ $fair == 2 ]]
then
  echo "-- $(realFlip)"
  echo "-- $(alternative) | alternate=true"
else
  echo "Invalid Configuration!"
fi

echo "Flip another coin. | refresh=true"
