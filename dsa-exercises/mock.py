# https://www.youtube.com/watch?v=MzdSvo42Blo
# an array of transactions
# a single string, comma separated
# name,time,city

# "alice,50,sf"
# flag (count) sus transactions

# sus if
# 2 transactions with the same name <= 60 min apart in dif cities
# "alice,10,sf" | "alice,40,ny" --> invalid, add both strings

# return the full transaction string if invalid
# input can be empty

# FOR REFERENCE I WATCHED THE FIRST 4 MINS ONLY
# additional question id ask
# will the transactions be ordered based on time (for this i'll assume yes)
def validate_transactions(transactions):
    # empty case
    if not transactions:
        return []

    # history of transactions per client
    client_transactions = {}
    invalid_transactions = []

    for client in transactions:
        name, time, city = client.split(',')
        if name not in client_transactions:
            # city lower to prevent issues if people write it diffferently
            client_transactions[name] = [[time, city.lower()]]
        else:
            # compare to most recent time
            most_recent_trans = client_transactions[name][-1]
            # print(most_recent_trans)
            # print(time)
            # because they are ordered by time, i can just check against the last record and get time difference
            time_since = int(time) - int(most_recent_trans[0])
            # once i get time difference
            # if over 60? fine, add it
            if time_since >= 60:
                client_transactions[name].append([time, city])
            # less than 60? is it in the same town? if yes, add to records. if not, pop and additional and this one
            else:
                if city.lower() == most_recent_trans[1]: continue
                # i need to reconvert this to the original string
                # no need to validate popping, since the initial else validates theres at least one transaction
                transaction_removed = client_transactions[name].pop()
                trans_string = name + ',' + ','.join(transaction_removed)
                # invalid_transactions.append(most_recent_transaction)
                invalid_transactions.append(client)
                invalid_transactions.append(trans_string)
        # print(client_transactions)
        # print(name, time, city)
    return invalid_transactions
transactions = ["alice,50,sf", "alice,60,ny"]
# print(validate_transactions(transactions))

# ---------------------------------------------------------------------------------------------
# bunch of classes --> a number
# youre given a list of pre reqs
# as many as i want
# pre req [1, 0] --> take 0 before 1
# min number of semesters i need to graduate

# ----------- my questions (stopped at 25mins)
# is my pre reqs just a list read right to left
# if i have 7, [0, 1, 2, 3] 
# does this --> 0 is pr to 1 | 1 is pr to 2 | 2 is pr to 3?
# meaning i have at least 4 semesters
# can i have a list of lists? ie 
# n = 6 but class 0 is pr for 1, and class 4 is pr for 5
# would looking something like [[0 1] [4 5]] 
# or, i can just pair them, and have a single list (like i did in first comment ig)

# val is the course, 
# children is the prerreqs, meaning all children must be counted beforehand

# so if i have something like 
# [[0 1] [0 2] [0 3] [2 4]]
# my tree would be
#   0      
# / | \    
# 1 2 3
# |
# 4

def min_semesters(n, prerreqs):
    # shove everything into one semster
    if not prerreqs: return 1
    
    reqs = {}
    # i need to countinously get from 0 to n
    taken_courses = set()
    # map course to its prereqs
    for pr in prerreqs:
        if pr[1] not in reqs:
            reqs[pr[1]] = [pr[0]]
        else:
            reqs[pr[1]].append(pr[0])
        taken_courses.add(pr[0])
        taken_courses.add(pr[1])

    # len is fine, bc i can just take prereqs + courses w/o prereqs simulataneously 

    # what i mean is sure i can take 3030 for 3033, along with art 101
    # in a single semester, unrelated classes
    return len(reqs)
    


print(min_semesters(7, [[0, 1], [4]]))