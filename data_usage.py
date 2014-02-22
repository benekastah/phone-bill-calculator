
import math

data_usage = {
    'Paul': {
        'December': 1401,
        'November': 1292,
        'October': 1362,
        'September': 1135,
        'August': 980,
        'July': 607,
        'June': 728,
        'May': 481,
        'April': 1796,
        'March': 928,
    },
    'Steve': {
        'December': 2092,
        'November': 1924,
        'October': 2890,
        'September': 1330,
        'August': 3669,
        'July': 3429,
        'June': 1096,
        'May': 812,
        'April': 2690,
        'March': 1462,
    },
    'Lindsey': {
        'December': 205,
        'November': 724,
        'October':  111,
        'September': 130,
        'August': 121,
        'July': 294,
        'June': 264,
        'May': 222,
        'April': 150,
        'March': 176,
    },
    'Rob': {
        'December': 2309,
        'November': 2522,
        'October': 2077,
        'September': 1125,
        'August': 858,
        'July': 237,
        'June': 1454,
        'May': 1256,
        'April': 330,
        'March': 2473,
    },
}

def mb_to_gb(mb):
    return float(mb) / 1024

def show_number(mb):
    return '%iMB (%.1fGB)' % (round(mb), mb_to_gb(mb))

def average(ls):
    return float(sum(ls)) / len(ls)

def median(data):
    sorted_data = sorted(data.values())
    l = len(sorted_data)
    m = l / 2
    if l % 2 != 0:
        return average(sorted_data[m:m+1])
    else:
        return sorted_data[m]

def analyze_usage(data):
    min_key = min(data, key=data.get)
    min_val = data[min_key]
    max_key = max(data, key=data.get)
    max_val = data[max_key]
    med = median(data)
    avg = average(data.values())
    rng = data[max_key] - data[min_key]
    return {
        'min': {'key': min_key, 'value': show_number(min_val)},
        'max': {'key': max_key, 'value': show_number(max_val)},
        'median': show_number(med),
        'average': show_number(avg),
        'range': show_number(rng),
    }

if __name__ == '__main__':
    print 'Data usage stats:'
    for name, data in data_usage.iteritems():
        a = analyze_usage(data)
        lines = [
            '%s:' % name,
            'Minimum usage: %(value)s in %(key)s' % a['min'],
            'Maximum usage: %(value)s in %(key)s' % a['max'],
            'Median usage: %(median)s' % a,
            'Average usage: %(average)s' % a,
            'Range: %(range)s' % a,
            '',
        ]
        print '\n  '.join(lines)

