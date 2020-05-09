#!/usr/bin/env python
# coding: utf-8

# In[1]:


# Install Libraries
from sodapy import Socrata # pip install sodapy
import os
import requests
import csv
import pandas as pd
import numpy as np
from pandas.api.types import CategoricalDtype
from plotnine import * # pip install plotnine
from plotnine import ggplot, geom_point, aes, stat_smooth, facet_wrap
import folium
import webbrowser
import matplotlib.pyplot as plt
plt.style.use('ggplot')
#get_ipython().run_line_magic('matplotlib', 'inline')
import warnings
warnings.simplefilter('ignore')


# In[2]:


import warnings
warnings.simplefilter('ignore')

# Load the API and file with the API supplied by Socrata

client = Socrata("data.cityofnewyork.us", None)

# Example authenticated client (needed for non-public datasets):
# client = Socrata(data.cityofnewyork.us,
#                  MyAppToken,
#                  userame="user@example.com",
#                  password="AFakePassword")

'''
#####----- DCA Inspections Database (New York City Inspections) -----#####
#####----- https://data.cityofnewyork.us/Business/Inspections/jzhd-m6uv
'''
# Can limit but setting at 1M to pull all, returned as JSON from API / converted to Python list of
# dictionaries by sodapy.
inspectionresults = client.get("jzhd-m6uv", limit=1000000)
infile = inspectionresults

# Convert to pandas DataFrame
inspections_df = pd.DataFrame.from_records(inspectionresults)

# Display results
nrows = len(inspections_df)
size = inspections_df.size
ncols = int(size/nrows)
print("*"*80)
print ("You loaded a total of", nrows, "records into your dataframe.")
print ("You have", nrows, "rows and", ncols ,"columns. Total data points is", size)
print("*"*80)
inspections_df.head(5)
  


# In[3]:


#####-----     Data Cleanup/Corrections     -----#####

# Convert to pandas DataFrame
originaldata = pd.DataFrame.from_records(inspectionresults)
inspections_df = pd.DataFrame.from_records(inspectionresults)

###--- DROP COLUMNS ---###
# Drop Junk Columns
inspections_df = inspections_df.drop(['description', 'unit_type', 'unit', 'street_2'], axis=1)

# Drop Unused Columns
inspections_df = inspections_df.drop(['certificate_number', 'building_number', 'street', 'city', 'state', 'zip'], axis=1)

#--- DROP NAs ---#
inspections_df = inspections_df.dropna(axis = 0, how ='any') 

###--- DATE ---###
#Convert Date column
inspections_df['inspection_date'] = pd.to_datetime(inspections_df['inspection_date'])

#Adding Year, Month and Weekday as categorical columns
inspections_df['yrmon'] = (inspections_df['inspection_date'].dt.strftime('%Y-%m')).astype('category')
inspections_df['year'] = (inspections_df['inspection_date'].dt.year).astype('category')
inspections_df['month'] = (inspections_df['inspection_date'].dt.month).astype('category')
inspections_df['weekday'] = (inspections_df['inspection_date'].dt.day_name()).astype('category')
inspections_df['inspection_date'] = inspections_df['inspection_date'].dt.strftime('%Y-%m-%d')

###--- Inspections & Violations ---###
# Correct Confiscated Licenses
inspections_df.inspection_result = inspections_df.inspection_result.replace({"Confiscated": "License Confiscated"})

# new column to categorize inspection results as violation or no violation
inspections_df = inspections_df.assign(violation = inspections_df['inspection_result'])
inspections_df.violation = inspections_df.violation.replace({"License Confiscated": "Violation Issued"})
inspections_df.loc[inspections_df["violation"] != "Violation Issued", "violation"] = "No Violation"
inspections_df['violation'].astype('category')

###--- BOROUGH ---###

# Replace Dublicates with Spelling Differences
inspections_df.borough = inspections_df.borough.replace({"MANHATTAN": "Manhattan",
                                                   "BRONX": "Bronx",
                                                   "QUEENS": "Queens",
                                                   "BROOKLYN": "Brooklyn"})

###--- Lat/Lon as floats ---###
inspections_df['latitude'] = inspections_df['latitude'].astype(float)
inspections_df['longitude'] = inspections_df['longitude'].astype(float)

###--- Liscense Categories ---###

# Categories with name changes
inspections_df.industry = inspections_df.industry.replace({"Cigarette Retail Dealer - 127": "Tobacco Retail Dealer",
                                                           "Laundry - 064": "Laundries",
                                                           "Laundry Jobber - 066": "Laundries"})

# Final cleaned set sample, updated stats
nrows = len(inspections_df)
size = inspections_df.size
ncols = int(size/nrows)
orows = len(originaldata)
osize = originaldata.size
ocols = int(osize/orows)

# Display results
print("*"*80)
print("You had", orows, "with", ocols, "columns and", osize, "datapoints.")
print ("You now have", nrows, "rows and", ncols ,"columns. Total data points is", size)
print ("You dropped",(orows-nrows),"rows")
print("*"*80)
print(inspections_df.dtypes)
inspections_df.head(5)

# Display results
#print("*"*80)
#print ("You have", nrows, "rows and", ncols ,"columns. Total data points is", size)
#print("*"*80)
#print(inspections_df.dtypes)
#inspections_df.head(5)


# In[4]:


# Inspections DF
inspections_df.to_csv('inspections.csv', index=False)
infile = 'inspections.csv'

# Inpections Summaries
# creating category_summarization function which will take the list of directories from the inspections file
# and the name pf one field. It then prints the categorical summary for that field.
def category_summarization(countrylist, fieldname):
    valuelist = []
    for inspection in inspectionList:
        valuelist.append (inspection[fieldname])
            
    # report the number of categories and the number of rows per category
    # the number of categories is the number of unique items, the set type gives us that
    categories = set(valuelist)
    numcategories = len(categories)

    # the number of items of each category is given by the count function
    # print these out for each category
    print('Number of categories', numcategories)
    
    # create a list of inspections in each category with their count for sorting
    categoryList = []
    
    for cat in categories:
        # adds a 4 tuple to the list
        categoryList.append((fieldname, cat, valuelist.count(cat),
                             "{:.2%}".format((int(valuelist.count(cat))/int(len(valuelist))))
                            ))
    # sort the categories by the field value, which is at index 2
    newlist = sorted(categoryList, key=lambda item: item[2], reverse=True)
    # print the sorted inspections
    
    for item in newlist:
        print( 'Field {:s} with Category {:s} and has {:d} entries for {:s}'.format(item[0],item[1],item[2],item[3])) 
    # end of function definition


# In[5]:


# create new empty list
inspectionList = []

with open(infile, 'rU') as csvfile:
    # the csv file reader returns a list of the csv items on each line
    inspecReader = csv.reader(csvfile,  dialect='excel', delimiter=',')
    # from each line, a list of row items, put each element in a dictionary
    #   with a key representing the data
    for line in inspecReader:
      # skip lines without data
      if line[0] == '' or line[0].startswith('record'):
          continue
      else:
          try:
            # create a dictionary for each Inspection
            inspection = {}
            # add each piece of data under a key representing that data
            inspection['recordID'] = line[0]
            inspection['businessName'] = line[1]
            inspection['InspecDate'] = line[2]
            inspection['InspecResult'] = line[3]
            inspection['Industry'] = line[4]
            inspection['borough'] = line[5]
            inspection['log'] = line[6]
            inspection['lat'] = line[7]
            inspection['yearmonth'] = line[8]
            inspection['year'] = line[9]
            inspection['month'] = line[10]
            inspection['weekday'] = line[11]
            inspection['violation'] = line[12]

            # add this inspection to the list
            inspectionList.append(inspection)

          except IndexError:
            print ('Error: ', line)
csvfile.close()


# In[6]:


# print summary of files read
print("*"*80)
print ("Read", len(inspectionList), "inspection data")
print("*"*80)

# all the fields except for the 'name' field
fieldnames = ['InspecResult', 'Industry','weekday', 'borough', 'year','violation']

for fieldname in fieldnames:
    category_summarization(inspectionList, fieldname)
    print()
    print("*"*80)


# In[8]:


# Create Violations Set
violations_df = inspections_df[(inspections_df['violation']=='Violation Issued')]
violations_df.to_csv('violations.csv', index=False)
infile = 'violations.csv'


# In[9]:


def violation_summarization(countrylist, fieldname):
    valuelist = []
    for violation in violationList:
        valuelist.append (violation[fieldname])
            
    categories = set(valuelist)
    numcategories = len(categories)
    print('Number of categories', numcategories)   

    categoryList = []
    
    for cat in categories:
        categoryList.append((fieldname, cat, valuelist.count(cat),
                             "{:.2%}".format((int(valuelist.count(cat))/int(len(valuelist))))
                            ))
    newlist = sorted(categoryList, key=lambda item: item[2], reverse=True)

    
    for item in newlist:
        print( 'Field {:s} with Category {:s} and has {:d} entries for {:s}'.format(item[0],item[1],item[2],item[3])) 
    # end of function definition


# In[10]:


violationList = []

with open(infile, 'rU') as csvfile:
    violReader = csv.reader(csvfile,  dialect='excel', delimiter=',')
    for line in violReader:
      if line[0] == '' or line[0].startswith('record'):
          continue
      else:
          try:
            # create a dictionary for each Violation
            violation = {}
            # add each piece of data under a key representing that data
            violation['recordID'] = line[0]
            violation['businessName'] = line[1]
            violation['InspecDate'] = line[2]
            violation['InspecResult'] = line[3]
            violation['Industry'] = line[4]
            violation['borough'] = line[5]
            violation['log'] = line[6]
            violation['lat'] = line[7]
            violation['yearmonth'] = line[8]
            violation['year'] = line[9]
            violation['month'] = line[10]
            violation['weekday'] = line[11]
            violation['violation'] = line[12]

            # add this violation to the list
            violationList.append(violation)

          except IndexError:
            print ('Error: ', line)
csvfile.close()


# In[11]:


# print summary of files read
print("*"*80)
print ("Read", len(violationList), "violation data")
print("*"*80)

# all the fields except for the 'name' field
fieldnames = ['Industry','weekday', 'borough', 'year','violation']

for fieldname in fieldnames:
    violation_summarization(violationList, fieldname)
    print()
    print("*"*80)


# In[55]:


print("*"*80)
print("Top 10 Industry types (Inspection set)")
print("*"*80)
print(inspections_df["industry"].value_counts().head(10))
print("*"*80)
#print(df_2017)


# In[13]:


print("*"*80)
print("Top 10 Industry types (Violation set)")
print("*"*80)
print(violations_df["industry"].value_counts().head(10))
print("*"*80)
#print(df_2017)


# In[52]:


print("*"*80)
print("Top 10 Inspection Results")
print("*"*80)
print(inspections_df["inspection_result"].value_counts().head(10))
print("*"*80)


# In[41]:


#####-----     Plot Inspections by Borough     -----#####
# Borough counts
boros = np.array(['Bronx','Brooklyn', 'Manhattan', 'Outside NYC', 'Queens', 'Staten Island'])
boroughset = inspections_df.filter(['record_id','borough','violation'], axis=1)
boroughcounts = boroughset.groupby(['borough','violation']).count()

# Count totals
boroughtotal = boroughcounts.sum(level=0)
boroughcounts = boroughcounts.unstack(level=1)
boroughcounts.columns = boroughcounts.columns.droplevel(level=0)
boroughcounts = boroughcounts.fillna(0)
boroughcounts.sort_values(by=['No Violation'])

# Print Summary
print("*"*80)
print (boroughcounts)
print("*"*80)

# Sorting
borough_list = inspections_df['borough'].value_counts().index.tolist()
borough_cat = pd.Categorical(inspections_df['borough'], categories=borough_list)

# assign to a new column in the DataFrame
inspections_df = inspections_df.assign(borough_cat = borough_cat)

# combine the counts and percentages
def combine(counts, percentages):
    fmt = '{} ({:.1f}%)'.format
    return [fmt(c, p) for c, p in zip(counts, percentages)]

# Plot Bar graph
g = (ggplot(inspections_df, aes('factor(borough_cat)', fill='violation'))         # defining what data to use
 + aes(x= 'borough_cat')    # defining what variable to use
 + geom_bar(size=100) # defining the type of plot to use
 + theme(axis_text_x = element_text(angle = 45, hjust = 1))
 + geom_text(
     aes(label='stat(combine(count, 100*prop))', group=1),
     stat='count', nudge_y=0.125, size=7, va='bottom')
 + labs(title='Number of Inspections By Borough', x='NYC Boroughs', y='Number of Inspections') # customizing labels
 + scale_fill_manual(values = ("Blue","Red"))
)
g.draw()


# In[43]:


#####-----     Plot Inspections by Day of of the week   -----#####
# Weekday counts
weekdays = np.array(['Sunday','Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'])
weekset = inspections_df.filter(['record_id','weekday','violation'], axis=1)
weekcounts = weekset.groupby(['weekday','violation']).count()

# Count totals
weektotal = weekcounts.sum(level=0)
weekcounts = weekcounts.unstack(level=1)
weekcounts.columns = weekcounts.columns.droplevel(level=0)
weekcounts = weekcounts.fillna(0)
weekcounts.sort_values(by=['No Violation'])

# Print Summary
print("*"*80)
print (weekcounts)
print("*"*80)
 
# Make Day of the Week Ordered Categorical
inspections_df['weekday'] = pd.Categorical(inspections_df['weekday'], categories=
['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'],
ordered=True)

# Plot Bar graph
g = (ggplot(inspections_df, aes('factor(weekday)', fill='violation'))         # defining what data to use
 + aes(x= 'weekday')    # defining what variable to use
 + geom_bar(size=100) # defining the type of plot to use
 + theme(axis_text_x = element_text(angle = 45, hjust = 1))
 + geom_text(
     aes(label='stat(combine(count, 100*prop))', group=1),
     stat='count', nudge_y=0.125, size=6, va='bottom')
 + labs(title='Number of Inspections By Weekday', x='Day of the Week', y='Number of Inspections')
 + scale_fill_manual(values = ("Blue","Red"))
)
g.draw()


# In[50]:


#####-----     Plot Inspections by Year  -----#####
# Year counts
years = np.array(['2017','2018', '2019'])
yearset = inspections_df.filter(['record_id','year','violation'], axis=1)
yearcounts = yearset.groupby(['year','violation']).count()

# Count totals
yeartotal = yearcounts.sum(level=0)
yearcounts = yearcounts.unstack(level=1)
yearcounts.columns = yearcounts.columns.droplevel(level=0)
yearcounts = yearcounts.fillna(0)
yearcounts.sort_values(by=['No Violation'])

# Print Summary
print("*"*80)
print (yearcounts)
print("*"*80)

# Plot Bar graph
g= (ggplot(inspections_df, aes('factor(year)', fill='violation'))         # defining what data to use
 + aes(x= 'year')    # defining what variable to use
 + geom_bar(size=100) # defining the type of plot to use
 + theme(axis_text_x = element_text(angle = 45, hjust = 1))
 + geom_text(
     aes(label='stat(count)', group=1),
     stat='count', nudge_y=0.125, size=6, va='bottom')
 + labs(title='Number of Inspections By Year', x='Year', y='Number of Inspections') # customizing labels
 + scale_fill_manual(values = ("Blue","Red"))
)
g.draw()


# In[51]:


#####-----     Plot Inspections by Month  -----##### 
# Year counts
months = np.array(['1','2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'])
monthset = inspections_df.filter(['record_id','month','violation'], axis=1)
monthcounts = monthset.groupby(['month','violation']).count()

# Count totals
monthtotal = monthcounts.sum(level=0)
monthcounts = monthcounts.unstack(level=1)
monthcounts.columns = monthcounts.columns.droplevel(level=0)
monthcounts = monthcounts.fillna(0)
monthcounts.sort_values(by=['No Violation'])

# Print Summary
print("*"*80)
print (monthcounts)
print("*"*80)

g = (ggplot(inspections_df, aes('factor(month)', fill='violation'))         # defining what data to use
 + aes(x= 'month')    # defining what variable to use
 + geom_bar(size=50) # defining the type of plot to use
 + theme(axis_text_x = element_text(angle = 45, hjust = 1))
 + geom_text( aes(label='stat(combine(count, 100*prop))', group=1),
     stat='count', nudge_y=0.125, size=5, va='bottom')
 + labs(title='Number of Inspections By month', x='month', y='Number of Inspections') # customizing labels
 + scale_fill_manual(values = ("Blue","Red"))
)
g.draw()


# In[23]:


#####-----     Plot Inspections by Month for each Year -----##### 
g = (ggplot(inspections_df, aes('factor(month)', fill='violation'))         # defining what data to use
+ aes(x= 'month')    # defining what variable to use
+ geom_bar(size=50) # defining the type of plot to use
+ facet_wrap("year")
+ theme(axis_text_x = element_text(angle = 90, hjust = 1))
+ geom_text( aes(label='stat(combine(count, 100*prop))', group=1),
    stat='count', nudge_y=0.125, size=5, va='bottom')
+ labs(title='Number of Inspections By month', x='month', y='Number of Inspections') # customizing labels
+ scale_fill_manual(values = ("Blue","Red"))
)
g.draw()


# In[25]:


# Daily comparison per year
#Select the columns we want
dailygroup = inspections_df[['inspection_date', 'borough', 'yrmon', 'year', 'month', 'weekday', 'violation']]
dailygroup['month']=dailygroup.month.astype('int64')

dailygroup['DATE_OF_INSPECTION'] = dailygroup['inspection_date']
dailygroup=dailygroup.set_index('DATE_OF_INSPECTION')
dailygroup = dailygroup.sort_index()


# In[26]:


# Create group DFs for each year grouped by date
dailygroup['daily_sum'] = 1
group2018= dailygroup.groupby(dailygroup.index)
df_2018=pd.DataFrame(group2018['daily_sum'].sum())
df_2018['cum_sum'] = df_2018.daily_sum.cumsum()
df_2018['day'] = range(len(df_2018))

group_inspection=dailygroup.groupby(['year'])

df2019=group_inspection.get_group(2019)
group2019= df2019.groupby(df2019.index)
df_2019=pd.DataFrame(group2019['daily_sum'].sum())
df_2019['cum_sum'] = df_2019.daily_sum.cumsum()
df_2019['day'] = range(len(df_2019))
df_2019.insert(3, 'year', '2019')
print(df_2019)

df2018=group_inspection.get_group(2018)
group2018= df2018.groupby(df2018.index)
df_2018=pd.DataFrame(group2018['daily_sum'].sum())
df_2018['cum_sum'] = df_2018.daily_sum.cumsum()
df_2018['day'] = range(len(df_2018))
df_2018.insert(3, 'year', '2018')
print(df_2018)

df2017=group_inspection.get_group(2017)
group2017= df2017.groupby(df2017.index)
df_2017=pd.DataFrame(group2017['daily_sum'].sum())
df_2017['cum_sum'] = df_2017.daily_sum.cumsum()
df_2017['day'] = range(len(df_2017))
df_2017.insert(3, 'year', '2017')
print(df_2017)


# In[28]:


# Creating Plot of Total Inspections in the Year (Using matplotlib.pyplot)

# Scatter plots.
ax1= df_2017.plot(kind='scatter', x='day',y='cum_sum', color='red',alpha=0.5, figsize=(10,5))
df_2018.plot(kind='scatter', x='day',y='cum_sum', color='orange',alpha=0.5, figsize=(10,5),ax=ax1)
df_2019.plot(kind='scatter', x='day',y='cum_sum', color='blue',alpha=0.5, figsize=(10,5),ax=ax1)

#Best fit polynomials for regression lines
df2017_fit = np.polyfit(df_2017.day,df_2017.cum_sum,1) #[ 239.50123034 1603.79033589]
df2018_fit = np.polyfit(df_2018.day,df_2018.cum_sum,1) #[219.41454971 -14.92260933]
df2019_fit = np.polyfit(df_2019.day,df_2019.cum_sum,1) #[217.60771658 299.24636316]

# Regression equations.
plt.text(175,70000,'y={:.2f}+{:.2f}*x'.format(df2017_fit[1],df2017_fit[0]),color='red',size=12)
plt.text(260,51000,'y={:.2f}+{:.2f}*x'.format(df2018_fit[1],df2018_fit[0]),color='orange',size=12)
plt.text(190,39000,'y={:.2f}+{:.2f}*x'.format(df2019_fit[1],df2019_fit[0]),color='blue',size=12)

# Legend, title and labels.
plt.legend(labels=['2017 Inspections','2018 Inspections', '2019 Inspections'])
plt.title('Total Annual Inspections', size=24)
plt.xlabel('Day Number in the Year (Out of 365)', size=18)
plt.ylabel('Cummulative Inspections', size=18);
plt.show()


# In[29]:


#YTD Graph Up until end of august
YTDgroup = inspections_df[['inspection_date', 'borough', 'yrmon', 'year', 'month', 'weekday', 'violation']]
YTDgroup['month']=YTDgroup.month.astype('int64')
YTDgroup = YTDgroup[YTDgroup['month']<8]

YTDgroup['DATE_OF_INSPECTION'] = YTDgroup['inspection_date']
YTDgroup=YTDgroup.set_index('DATE_OF_INSPECTION')
YTDgroup = YTDgroup.sort_index()


# Create group DFs
YTDgroup['daily_sum'] = 1
group2018= YTDgroup.groupby(YTDgroup.index)
YTD2018=pd.DataFrame(group2018['daily_sum'].sum())
YTD2018['cum_sum'] = YTD2018.daily_sum.cumsum()
YTD2018['day'] = range(len(YTD2018))

group_inspection=YTDgroup.groupby(['year'])

df2019=group_inspection.get_group(2019)
group2019= df2019.groupby(df2019.index)
YTD2019=pd.DataFrame(group2019['daily_sum'].sum())
YTD2019['cum_sum'] = YTD2019.daily_sum.cumsum()
YTD2019['day'] = range(len(YTD2019))
YTD2019.insert(3, 'year', '2019')
print(YTD2019)
print("*"*80)

df2018=group_inspection.get_group(2018)
group2018= df2018.groupby(df2018.index)
YTD2018=pd.DataFrame(group2018['daily_sum'].sum())
YTD2018['cum_sum'] = YTD2018.daily_sum.cumsum()
YTD2018['day'] = range(len(YTD2018))
YTD2018.insert(3, 'year', '2018')
print(YTD2018)
print("*"*80)

df2017=group_inspection.get_group(2017)
group2017= df2017.groupby(df2017.index)
YTD2017=pd.DataFrame(group2017['daily_sum'].sum())
YTD2017['cum_sum'] = YTD2017.daily_sum.cumsum()
YTD2017['day'] = range(len(YTD2017))
YTD2017.insert(3, 'year', '2017')
print(YTD2017)
print("*"*80)


# In[32]:


# Creating Plot of Total Inspections in the Year

# Scatter plots.
ax1= YTD2017.plot(kind='scatter', x='day',y='cum_sum', color='red',alpha=0.5, figsize=(10,5))
YTD2018.plot(kind='scatter', x='day',y='cum_sum', color='orange',alpha=0.5, figsize=(10,5),ax=ax1)
YTD2019.plot(kind='scatter', x='day',y='cum_sum', color='blue',alpha=0.5, figsize=(10,5),ax=ax1)

#Best fit polynomials for regression lines
YTD2017_fit = np.polyfit(YTD2017.day,YTD2017.cum_sum,1) #[248.54346118 733.58163837]
YTD2018_fit = np.polyfit(YTD2018.day,YTD2018.cum_sum,1) #[217.31322235 144.54155999]
YTD2019_fit = np.polyfit(YTD2019.day,YTD2019.cum_sum,1) #[217.60771658 299.24636316]

# Regression equations.
plt.text(110,45000,'y={:.2f}+{:.2f}*x'.format(YTD2017_fit[1],YTD2017_fit[0]),color='red',size=12)
plt.text(150,30000,'y={:.2f}+{:.2f}*x'.format(YTD2018_fit[1],YTD2018_fit[0]),color='orange',size=12)
plt.text(100,20000,'y={:.2f}+{:.2f}*x'.format(YTD2019_fit[1],YTD2019_fit[0]),color='blue',size=12)

# Legend, title and labels.
plt.legend(labels=['2017 Inspections','2018 Inspections', '2019 Inspections'])
plt.title('Total Annual Inspections \nJanuary - July', size=24)
plt.xlabel('Day Number in the Year (Out of 365)', size=18)
plt.ylabel('Cummulative Inspections', size=18);
plt.show()


# In[54]:


#Mapping Datasets - Tobacco Inspections for July 2019

# Pull Violations Data 
violations_df = inspections_df[(inspections_df['inspection_result']=='Violation Issued')]
violations_df = violations_df[(violations_df['industry']=='Tobacco Retail Dealer')]
#violations_df = violations_df[(violations_df['industry']=='Tobacco Retail Dealer') | (violations_df['industry']=='Grocery-Retail')]
violations_df = violations_df[(violations_df['year']==2019) & (violations_df['month']==7)]
#violations_df = violations_df[(violations_df['year']==2019)]
violations_df = violations_df[['latitude', 'longitude','business_name','industry']]
violations_df = violations_df.dropna(axis=0, subset=['latitude','longitude'])

# Pull Inspec Data
nov_df = inspections_df[inspections_df['inspection_result']!='Violation Issued']
nov_df = nov_df[(nov_df['industry']=='Tobacco Retail Dealer')]
#nov_df = nov_df[(nov_df['industry']=='Tobacco Retail Dealer') | (nov_df['industry']=='Grocery-Retail')]
nov_df = nov_df[(nov_df['year']==2019) & (nov_df['month']==7)]
#nov_df = nov_df[(nov_df['year']==2019)]
nov_df = nov_df[['latitude', 'longitude','business_name','industry']]
nov_df = nov_df.dropna(axis=0, subset=['latitude','longitude'])

# NYCmap
nycmap = folium.Map(
    location=[40.713050, -74.007230],
    zoom_start=11)

# Add Violations marker one by one on the map
for i in range(0,len(violations_df)):
    folium.Marker([violations_df.iloc[i]['latitude'],
                   violations_df.iloc[i]['longitude']],
                  popup=(violations_df.iloc[i]['business_name'], violations_df.iloc[i]['industry']),
                  icon=folium.Icon(color='red', icon='remove')
                 ).add_to(nycmap)
    
# Add Non-Violations marker one by one on the map
for i in range(0,len(nov_df)):
    folium.Marker([nov_df.iloc[i]['latitude'],
                   nov_df.iloc[i]['longitude']],
                  popup=(nov_df.iloc[i]['business_name'], nov_df.iloc[i]['industry']),
                  icon=folium.Icon(color='blue', icon='thumbs-up')
                 ).add_to(nycmap)

print("*"*80)
print("Generating Tobacco Inspections Map for July 2019")
print("Plotting", len(violations_df), "Violation Tobacco Inspections")
print("Plotting", len(nov_df), "Non-Violation Tobacco inspections")
print("Click individual markers on the map for details")
print("*"*80)
nycmap


# In[40]:


output_file = "inspectionsmap.html"
map = nycmap
map.save(output_file)
webbrowser.open(output_file, new=2)


# In[ ]:




