# csci2322-p2
Project 2

On this project, you are not allowed to collaborate at the keyboard outside of your cheat group. Do
not look at other student's code.  You may collaborate on a whiteboard or on paper.  If you are
having problems with syntax or semantics errors, I encourage you to come to office hours.

Student Name:
Trinity ID: 

Milestone: 4/4
Core Project: 71/71
Complexities: 2/10
Avoiding Work: 12/12
Showdown: 3/3
Total: 92 (A)

Project Check: awk bestPlay Scavenge.hs
bestPlay:: Dictionary -> Hand -> Play 
bestPlay dictionary hand = snd (helper valid hand)
    where valid = validMoves dictionary hand 

Project Evaluation: --forceTests --quiet
Failed that canMake is linear: 
			Failed! Your solution is probably roughly quadratic. 
			Increasing input size from 1024 to 2048 increased the runtime from 0.228s to 1.059s, a factor of 4.642
Failed that validMoves is linear: 
			Failed! Your solution is probably roughly quadratic. 
			Increasing input size from 64 to 128 increased the runtime from 0.501s to 1.974s, a factor of 3.937
full credit: algorithm complexities: failed with 2/10
full credit tests: failed with 14/22
Project 2: failed with 85/93
TIME: 5.927
Project 2: failed with 85/93

Project Evaluation: -s best ALEFACNUMXHZLXW -q
Play (51): AL CWM FLEX HUN ZAX
TIME: 0.566

Project Evaluation: -s best -d2 ALEFACNUMXHZLXW -q
Play (25): A FALL MUCH NEW
TIME: 0.008

Project Evaluation: -s best -d1 NKFDEQURYPOVOFIOEFIX
Using game tree strategy with dictionary standardDict (4k)
Input hand: NKFDEQURYPOVOFIOEFIX
Total letter score: 58
Play found: DRY EQUIP OFF OK FIX OVEN
Score: 58
TIME: 0.130

Project Evaluation: -s best -q -d1 SELYPLVHDECUYDULCEEHYXYNA
Play (59): SEXY HEAVY UH PULL CYCLE DENY
TIME: 0.902

Project Evaluation: -s best -d1 -q MAGPISEYPABQKOOZPDXQHT
Play (58): DIP AH SKY BOX OPT GAZE PM
TIME: 4.184

Project Evaluation: -s best -d1 -q QSECFLEZKUDLEXVUGAYYTRIMY
Play (75): LAY LUCK DRY SQUEEZE FIX GYM TV
TIME: 27.799

Project Evaluation: -s best -d1 -q QNICLJGHFTJORYZGZICEOWBQZTGUSU
Play (68): LOG CHEF QUIT CRY WING JOB JUST
TIME: 170.921

