# set up, all import
import json
import helpers
import os
import matplotlib.pyplot as plt
from urllib.request import urlopen, HTTPError


# defining classes
class Data_file:
    """This class contains a json dictionary and methods to extract
    information about available variables. Some attributes are strings
    and other lists in order to ease printing, matching and interaction."""

    def __init__(self, file):
        """A Data_file's file is saved as an attribute (dict) in order to access it."""
        self.file = file

    def get_parameters(self):
        """Saves the parameters (str) as well as the parameters and
        their associated integers (list) as attributes."""
        parameters = ''
        parameter_integers = []
        for i in range(len(self.file['resource'])):
            integer = self.file['resource'][i]['key']
            parameter = self.file['resource'][i]['title']
            summary = self.file['resource'][i]['summary']
            if helpers.is_valid_argument(parameter):  # only parameters which are valid arguments are saved
                parameters += f'{integer}: {parameter}, {summary}\n'
                parameter_integers.append(int(integer))
        self.parameters = parameters
        self.parameter_integers = parameter_integers

    def get_stations(self):
        """Saves the stations (str) as well as the station ids (list) as attributes."""
        stations = ''
        station_ids = []
        for i in range(len(self.file['station'])):
            name = self.file['station'][i]['name']
            id = self.file['station'][i]['id']
            station_ids.append(id)
            stations += f'{id}: {name}\n'
        self.stations = stations
        self.station_ids = station_ids

    def get_periods(self):
        """Saves the periods and their associated integers (str),
        the integers (list) as well as the periods (list) as attributes."""
        periods_and_integers = ''
        period_integers = []
        periods = []
        for i in range(len(self.file['period'])):
            integer = i + 1
            period = self.file['period'][i]['key']
            if helpers.is_valid_argument(period):  # only periods which are valid arguments are saved
                periods_and_integers += f'{integer}: {period}\n'
                period_integers.append(integer)
                periods.append(period)
        self.periods_and_integers = periods_and_integers
        self.period_integers = period_integers
        self.periods = periods


class Data_url:
    """This class is specifically made for reading data from
     and building upon an API url."""

    def __init__(self):
        """Data_url starts with the attributes url (str)
        which contains information about the first node, parameter,
        also data (dict) where Data_file is incorporated by
        collecting Datafiles in Data_url attribute."""
        self.url = 'https://opendata-download-metobs.smhi.se/api/version/1.0.json'
        self.data = {}

    def _check_cached_(self):
        """Checks if file already exists in .cache map on computer.
        Returns answer (boolean) depending on which case and url_name (str)."""
        if os.path.exists('.cache'):  # making sure there is a .cache path
            pass
        else:
            os.makedirs('.cache')

        url_name = self.url.replace('/', '-')  # url_name is used for saving file on computer,
        url_name = url_name.replace(':', '-')  # can not contain these characters
        cached_files = os.listdir(path='.cache')

        if url_name in cached_files:
            return True, url_name
        else:
            return False, url_name

    def read_data(self):
        """Depending on answer from check cached,
        data will be read from cache map or SMHI API.
        Saved attribute is jsonfile (dict), which is overwritten each time,
        since only the last one is needed for data processing.
        Also, each Data_file is created and added to data attribute (dict)."""
        cached, url_name = self._check_cached_()
        if cached:
            with open(f'.cache/{url_name}', 'r') as fp:
                loaded_data = json.load(fp)
            print('File loaded from .cache map.')
        else:
            file = urlopen(self.url)
            loaded_data = json.load(file)
            with open(f'.cache/{url_name}', 'w') as fp:
                json.dump(loaded_data, fp)
                print('File loaded from SMHI API and saved in .cache map')

        data = Data_file(loaded_data)
        self.data[self.url] = data
        self.jsonfile = loaded_data

    def add_parameter(self, integer):
        """Adds node to url attribute (str), which contain information of next node."""
        self.url = f'https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/' \
                   f'{integer}.json'

    def add_station(self, integer, id):
        """Adds node to url attribute (str), which contain information of next node."""
        self.url = f'https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/' \
                   f'{integer}/station/{id}.json'

    def add_period(self, integer, id, period):
        """Adds node to url attribute (str).
        This will be the finished url to load the desired jsonfile from."""
        self.url = f'https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/' \
                   f'{integer}/station/{id}/period/{period}/data.json'

    def add_parameter_station_set(self, integer):
        """Adds parameter node to the url attribute (str).
        In this case station set and latest-hour is obligatory,
        this will be the finished url to load the desired jsonfile from."""
        self.url = f'https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/' \
                   f'{integer}/station-set/all/period/latest-hour/data.json'


# defining functions for part two
def extract_info_two(json_file):
    """Takes in json_file (dict) and returns information such as
    parameter (str), summary (str) and station (str)."""
    parameter = json_file['parameter']['name']
    summary = json_file['parameter']['summary']
    station_name = json_file['station']['name']
    station_key = json_file['station']['key']
    station = station_name + ' (' + station_key + ')'

    return parameter, summary, station


def save_dates_temps(json_file):
    """Takes in json_file (dict), returns lists of
    timestamps (int) and measurement values (int)."""
    timestamps = []
    values = []
    if json_file['value']:  # only does operation if there exist values
        for i in range(len(json_file['value'])):
            timestamps.append(json_file['value'][i]['date'])
            values.append(float(json_file['value'][i]['value']))
    else:
        print('SMHI gave this period as an option even though no values exist.')

    return timestamps, values


def sort_day_night(timestamps, values):
    """Divides timestamps and measurement values into separate
    dictionaries depending on period, day_measurements and night_measurements.
    Day period: 06 <= time o'clock < 18.
    Night period: 00 <= time o'clock < 06 or 18 <= time o'clock < 24.

    Parameters
    ---------
    timestamps, values: list of int
    """
    one = int(3.6 * 10 ** 6)  # each time o'clock in milliseconds
    six = 6 * one
    eighteen = 18 * one
    twentyfour = 24 * one

    day_measurements = {}
    night_measurements = {}
    for i in range(len(timestamps)):
        remainder = timestamps[i] % twentyfour  # gives hour of time in milliseconds by modulo operator
        if six <= remainder < eighteen:
            day_measurements[timestamps[i]] = [float(values[i])]  # sorts measurements occurring during day
        elif 0 <= remainder < six or eighteen <= remainder < twentyfour:
            night_measurements[timestamps[i]] = [float(values[i])]  # sorts measurements occurring during night

    return day_measurements, night_measurements


def gather_period_values(period_measurements):
    """Takes in period_measurements (dict), returns values_gathered (dict)
    with each key being that period (day/night) at twelve o'clock,
    values being list of those days or nights measurements."""
    one = int(3.6 * 10 ** 6)  # each time o'clock in milliseconds
    twelve = 12 * one
    twentyfour = 24 * one
    period_values = []  # will be used to collect each value until new period
    values_gathered = {}  # list of period values will be saved here

    if len(period_measurements) != 0:  # only does operations if values exist
        for i in range(len(period_measurements)):
            key = list(period_measurements.keys())[i]
            period_start = key - key % twentyfour
            key_before = list(period_measurements.keys())[i - 1]
            period_before = key_before - key_before % twentyfour

            if i != 0 and period_start != period_before:  # in case of new period, save earlier values
                values_gathered[period_before + twelve] = period_values
                period_values = []

            if period_measurements[key] != [None]:  # gather values only if there are any
                period_values += period_measurements[key]

        key_last = list(period_measurements.keys())[-1]
        period_last = key_last - key_last % twentyfour
        values_gathered[period_last + twelve] = period_values  # the last period has to be saved too

    return values_gathered


def count_averages(values_gathered):
    """Count averages for each key values in a dictionary,
    returns new dictionary of same keys but with averages as values."""
    period_averages = {}
    for key in values_gathered:
        values_sum = sum(values_gathered[key])
        total = len(values_gathered[key])  # since a key in values_gathered is only added if not empty,
        average = values_sum / total  # there wont be a zero division error.
        period_averages[key] = [average]

    return period_averages


def same_keys_dicts(dict, dict_decide):
    """Takes in dict and makes it contain same keys in same order as dict_decide."""
    dict = dict.copy()  # do not want to change original dictionaries
    dict_decide = dict_decide.copy()

    for key in list(dict_decide.keys()):  # dictionaries will contain same dates,
        if key not in list(dict.keys()):  # by adding a key and None value if not.
            dict[key] = [None]

    sorted_tuples = sorted(dict.items())  # dictionaries will have same order
    new_dict = {}
    for i in range(len(sorted_tuples)):
        new_dict[sorted_tuples[i][0]] = sorted_tuples[i][1]

    return new_dict


def convert_to_date(period_averages):
    """Takes in dictionary of which the keys are timestamps,
    converts timestamps to dates and saves in a new dictionary."""
    dates_averages = {}
    for key in list(period_averages.keys()):
        datetime = helpers.smhi_time_to_string(int(key))
        date = datetime.split()[0]
        dates_averages[f'{date}'] = period_averages[key]

    return dates_averages


def make_doc_str(day_averages, night_averages, station, parameter, summary):
    """Makes a csv document string containing a title (station, parameter and summary),
    as well as a table of each date and their averages for day and night.

    Parameters
    ----------
    day_averages, night_averages: dict
    station, parameter, summary: str
    """
    title = f'{station};{parameter};{summary}\nDatum,Medel dag,Medel natt\n'
    table = ''
    day_averages = same_keys_dicts(day_averages, night_averages)
    night_averages = same_keys_dicts(night_averages, day_averages)
    day_avg = convert_to_date(day_averages)
    night_avg = convert_to_date(night_averages)

    table_cont = [list(day_avg.keys()), list(day_avg.values()), list(night_avg.values())]
    for i in range(len(table_cont[0])):
        table += f'{table_cont[0][i]},{table_cont[1][i][0]},{table_cont[2][i][0]}\n'

    doc_str = title + table

    return doc_str


def make_csv(document_string):
    """Takes in document_string that will be writen to new_file,
    which the user choose name of."""
    while True:
        try:
            new_file = input('What would you like to name the new csv file? ')
            with open(f'{new_file}.csv', 'w') as fp:
                fp.write(document_string)
            break
        except OSError:
            print('Write in valid filename.\n')


def plot_measurements(
        timestamps, values, day_averages, night_averages, parameter, summary, station):
    """Plots values as well as day and night averages for each date.

    Parameters
    ----------
    timestamps, values: list of int
    day_averages, night_averages: dict
    parameter, summary: str
    """
    plt.plot(list(day_averages.keys()), list(day_averages.values()),
        'vr', label='Dagsmedel', alpha=0.7)
    plt.legend()

    plt.plot(list(night_averages.keys()), list(night_averages.values()),
             '^b', label='Nattmedel', alpha=0.7)
    plt.legend()

    plt.plot(timestamps, values, '.g', label='Timvärden', alpha=0.5)
    plt.legend()

    plt.xlabel('Datum')
    plt.ylabel(f'{parameter}')
    plt.title(f'{station}: {parameter} {summary}')
    plt.show()


# defining functions for part three
def extract_info_three(json_file):
    """Takes in json_file (dict) and returns information such as parameter,
    unit, timestamp (str), each lat, long and their values (lists)."""
    parameter = json_file['parameter']['name']
    unit = json_file['parameter']['unit']
    timestamp = json_file['period']['from']
    lat = []
    long = []
    values = []
    for i in range(len(json_file['station'])):
        station_value = json_file['station'][i]['value']
        if not station_value:  # does not add station with None values
            pass
        else:
            values.append(float(station_value[0]['value']))
            lat.append(json_file['station'][i]['latitude'])
            long.append(json_file['station'][i]['longitude'])

    return parameter, unit, timestamp, lat, long, values


def get_sweden_longlat():
    """Reads from a csv file, returns sweden_long and sweden_lat (lists),
    which will be the outline of Sweden's map in plot."""
    sweden_long = []
    sweden_lat = []
    with open('sweden_longlat.csv', 'r') as fp:
        for row in fp:
            row_numbers = row.removesuffix('\n')
            row_split = row_numbers.split(',')
            if row_split == ['Longitude', 'Latitude']:
                pass
            else:
                sweden_long.append(float(row_split[0]))
                sweden_lat.append(float(row_split[1]))

    return sweden_long, sweden_lat


def plotting_map(sweden_long, sweden_lat, long, lat, values, parameter, unit, timestamp):
    """Plots latest hour values for each station available.

    Parameters
    ----------
    sweden_long, sweden_lat, long, lat, values: list
    parameter, unit, timestamp: str
    """
    plt.plot(sweden_long, sweden_lat, 'k.', markersize=0.75)

    values_print = [i ** 2 for i in values]
    plt.scatter(long, lat, s=values_print, c=values, alpha=0.5)
    plt.colorbar()
    plt.xlabel('Longitude')
    plt.ylabel('Latitude')

    datetime = helpers.smhi_time_to_string(int(timestamp) - 1)
    plt.title(f'{parameter}({unit})\n{datetime}')
    plt.show()


# defining functions for interaction of part two and three
def ask_print_variable_options(what_to_print, variable):
    """Tells the user which variable is to be chosen,
    ask if to print options out or not.

    Parameters
    ----------
    what_to_print, variable: str
    """
    answer = input(f'Choose {variable}\n'
                   f'Would you like to see the available {variable}s? Answer yes or no: ')
    if answer != 'no':
        print(what_to_print)
    if answer != 'yes' and answer != 'no':
        print(f'Your answer was written incorrect, '
              f'it was assumed you wanted to see the {variable}s.')


def in_list(int, int_list, variable):
    """Checks if int is in int_list of available variable (str),
    returns True of False."""
    if int not in int_list:
        print(f'{int} is not in the list of available {variable}s. '
              'Please start over.\n')
        return False
    else:
        return True


# I'm aware that the interaction functions are a bit long,
# I choose to do so instead of dividing them,
# since it makes it more clear what's going on and error handling is smoother.
def interaction_program_two():
    """Interaction which makes use of the two classes Data_file and Data_url
     in order to load and return desired file."""
    while True:
        try:
            print('This program lets you choose parameter, station and period '
                  'for a json dataset,\nwhich will be processed to make a plot '
                  'and csv file of the mean measurements for each day and night.')

            smhi = Data_url()
            smhi.read_data()
            smhi.data[smhi.url].get_parameters()

            ask_print_variable_options(smhi.data[smhi.url].parameters, 'parameter')
            integer_prmt = int(input('Enter integer associated with parameter: '))
            if not in_list(integer_prmt, smhi.data[smhi.url].parameter_integers, 'parameter'):
                continue

            smhi.add_parameter(integer_prmt)
            smhi.read_data()
            smhi.data[smhi.url].get_stations()
            ask_print_variable_options(smhi.data[smhi.url].stations, 'station')
            id = int(input('Enter station id (integer) associated with station: '))
            if not in_list(id, smhi.data[smhi.url].station_ids, 'id'):
                continue

            smhi.add_station(integer_prmt, id)
            smhi.read_data()
            smhi.data[smhi.url].get_periods()
            if not smhi.data[smhi.url].periods:
                print('There are no data for any valid period in this case. '
                      'Please start over.\n')
                continue

            ask_print_variable_options(smhi.data[smhi.url].periods_and_integers, 'period')
            integer_prd = int(input('Enter integer associated with period: '))
            if not in_list(integer_prd, smhi.data[smhi.url].period_integers, 'period'):
                continue

            period = smhi.data[smhi.url].periods[integer_prd - 1]
            smhi.add_period(integer_prmt, id, period)
            smhi.read_data()
            break

        except HTTPError:
            print('URL does not exist, Please start over.\n')
        except ValueError:
            print('Please enter integer, you will have to start over.\n')

    return smhi.jsonfile


def interaction_program_three():
    """Interaction which makes use of the two classes Data_file and Data_url
     in order to load and return desired file."""
    while True:
        try:
            print('This program lets you choose parameter for a json dataset,\n'
                  'which will be processed to plot a map with instantaneous '
                  'values for all available stations.')

            smhi = Data_url()
            smhi.read_data()
            smhi.data[smhi.url].get_parameters()

            ask_print_variable_options(smhi.data[smhi.url].parameters, 'parameter')
            integer_prmt = int(input('Enter integer associated with parameter: '))
            if not in_list(integer_prmt, smhi.data[smhi.url].parameter_integers, 'parameter'):
                continue

            smhi.add_parameter_station_set(integer_prmt)
            smhi.read_data()
            break

        except HTTPError:
            print('URL does not exist, probably meaning that the parameter you chosen '
                  'does not have station-set available. Please start over.\n')
        except ValueError:
            print('Please enter integer, you will have to start over.\n')

    return smhi.jsonfile


# functions to run programs
def run_program_two():
    """Goes through the process of program two,
    which is loading data, wrangling it, plotting,
    and making a csv file with the information."""
    json_file = interaction_program_two()
    parameter, summary, station = extract_info_two(json_file)
    timestamps, values = save_dates_temps(json_file)
    day_measurements, night_measurements = sort_day_night(timestamps, values)
    day_gathered = gather_period_values(day_measurements)
    night_gathered = gather_period_values(night_measurements)
    day_avg = count_averages(day_gathered)
    night_avg = count_averages(night_gathered)
    doc_str = make_doc_str(day_avg, night_avg, station, parameter, summary)
    make_csv(doc_str)
    plot_measurements(timestamps, values, day_avg, night_avg, parameter, summary, station)


def run_program_three():
    """Goes through the process of program three,
    which is loading the data, extracting necessary
    information and plotting it on a map."""
    json_file = interaction_program_three()
    parameter, unit, timestamp, lat, long, values = extract_info_three(json_file)
    sweden_long, sweden_lat = get_sweden_longlat()
    plotting_map(sweden_long, sweden_lat, long, lat, values, parameter, unit, timestamp)


def main():
    """Lets user choose which program will be run."""
    while True:
        try:
            answer = int(input('Welcome to my SMHI Open Data project, '
                               'which one of the programs would you like to run, 2 or 3? Enter integer: '))
            if answer != 2 and answer != 3:
                print('Your answer was written incorrectly, please enter an 2 or 3.\n')
                continue
            elif answer == 2:
                run_program_two()
            elif answer == 3:
                run_program_three()

            answer = input('Would you like to start over? Enter yes or no: ')
            if answer != 'yes' and answer != 'no':
                print('Your answer was written incorrectly, '
                      'it is assumed you want to start over.')
                continue
            elif answer == 'yes':
                continue
            elif answer == 'no':
                print('Good bye!')
                break
        except ValueError:
            print('Please enter integer.\n')


# runs main
if __name__ == '__main__':
    main()
