#!/usr/bin/env python

import json

def show_dollars(f):
  return '${0:.2f}'.format(round(f, 2))

def get_float(prompt, default=0):
  print('%s (%s)' % (prompt, show_dollars(default)))
  return float(raw_input() or default)

# Get values from the AT&T bill
total = get_float('Total bill amount')
assert total > 0, 'Total is required'

# This is the same every month.
line_cost = get_float('Mobile Share', 30)

# The "other charges" area just happened to be the same for everybody.
# May not always be the case
other = get_float('Surcharges and fees') + \
    get_float('Government fees & taxes') + \
    get_float('Any other cost that applies to everyone equally')

# This data cost and discount are always added to steven's line and need
# to be divied up between all the lines. These should be the same every
# month
data = get_float('Data cost (Mobile Share 10GB with Unlimited Talk & Text)',
    120)
discount = get_float('National Account Discount', -26.4)
assert discount <= 0, 'discount should be less than or equal to 0'

# This is just an easy way to account for costs that only apply to some lines
# The letters are the first inital of each person's name
starting_costs = {
  'p': get_float('Paul\'s costs', 6.99),
  'r': get_float('Rob\'s costs'),
  'l': get_float('Lindesy\'s costs'),
  's': get_float('Steve\'s costs')}

# Caluclate the cost for one line
def cost_for(starting_cost):
  lines = len(starting_costs)
  return starting_cost + (data / lines) + (discount / lines) + line_cost + other

# Calls cost_for for each line, feeding in the starting_cost as a parameter
def get_costs():
  costs = {initial: cost_for(start) for initial, start in starting_costs.iteritems()}
  # Make sure the divied up result equals the total amount from the bill
  _total = sum(x for _, x in costs.iteritems())
  assert abs(total - _total) < 0.0001, '%f != %f' % (_total, total)
  return costs

costs = get_costs()

print('Paul\'s bill: %s' % show_dollars(costs['p']))
print('Steve\'s bill: %s' % show_dollars(costs['s']))
print('Rob & Lindsey\'s bill: %s (%s + %s)' % (
  show_dollars(costs['r'] + costs['l']),
  show_dollars(costs['r']),
  show_dollars(costs['l'])))

