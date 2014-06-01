#!/usr/bin/env python

from collections import defaultdict
import datetime
import sys

import yaml

def get_payup_data(payup_fname):
    return yaml.load(file(payup_fname))

MONTHS = dict((v, i + 1) for i, v in enumerate([
    'jan',
    'feb',
    'mar',
    'apr',
    'jun',
    'jul',
    'aug',
    'sep',
    'oct',
    'nov',
    'dec',
    ]))

def get_year_month(ym):
    year, month = ym.split('-')
    return datetime.date(int(year), MONTHS[month.lower()], 23)

class Account(object):
    def __init__(self, name):
        self.name = name
        self.owes = 0
        self.paid = 0
        self._data = defaultdict(lambda: {})

    def add_owes(self, ym, owes):
        self.owes += owes
        data = self._data[ym]
        data['owes'] = owes

    def add_paid(self, ym, paid):
        self.paid += paid['amount']
        data = self._data[ym]
        data['paid'] = paid

    def process_data(self, ym, data):
        acct_data = self._data[ym]
        if not data:
            acct_data['warning'] = 'Missing all data'
        else:
            owes = data['owes']
            if owes is None:
                acct_data['warning'] = 'Missing "owes" data'
            else:
                self.add_owes(ym, owes)
            paid = data['paid']
            if not paid or paid.get('amount') is None:
                acct_data['warning'] = 'Missing "paid" data'
            else:
                self.add_paid(ym, paid)

    def warn(self):
        for key in sorted(self._data.iterkeys()):
            data = self._data[key]
            warning = data.get('warning')
            if warning:
                print('Warning {0}: {1}'.format(key, warning))

    def show_details(self):
        self.warn()
        if self.is_square:
            print('Account "{0}" is square'.format(self.name))
        elif self.discrepancy > 0:
            print('Account "{0}" owes {1}'.format(self.name, self.discrepancy))
        else:
            print('Account "{0}" overpaid by {1}'.format(self.name, abs(self.discrepancy)))
        print('')

    @property
    def discrepancy(self):
        return self.owes - self.paid

    @property
    def is_square(self):
        return self.discrepancy == 0

class AccountDict(dict):
    def __missing__(self, key):
        acct = Account(key)
        self[key] = acct
        return acct

def warn_missing_data(data_name, acct, when):
    print('Missing data "{0}" for account "{1}" on {2}'
            .format(data_name, acct.name, when))

def process_payup_data(payup_data):
    accounts = AccountDict()
    for ym, month_data in payup_data.iteritems():
        ym = get_year_month(ym)
        for acct_name, acct_data in month_data.iteritems():
            acct = accounts[acct_name]
            acct.process_data(ym, acct_data)
    for acct in accounts.values():
        acct.show_details()

if __name__ == '__main__':
    process_payup_data(get_payup_data(sys.argv[1]))
