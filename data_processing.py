import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import datetime
import time
import seaborn as sns

#read the original data
df = pd.read_csv('Data/af-historic-wildfires-2006-2018-data.csv',encoding = 'cp1252')

#preporecessing the data
#start by converting string type date columns into datetime format
date_time_cols = ['assessment_datetime','fire_start_date','discovered_date', 
'reported_date','start_for_fire_date','fire_fighting_start_date',
'bh_fs_date', 'uc_fs_date','to_fs_date','ex_fs_date']

df[date_time_cols] = df[date_time_cols].apply(pd.to_datetime, errors='coerce')
df['fire_id'] = df.index

#subset the columns needed in the data
cols_adj = ['fire_id','fire_number','calendar_year','fire_location_latitude','fire_location_longitude',
            'general_cause_desc', 'fire_type','size_class','fuel_type','assessment_datetime',
            'assessment_hectares','fire_start_date','discovered_date','reported_date','fire_fighting_start_date',
            'fire_fighting_start_size','bh_fs_date', 'bh_hectares','uc_fs_date','uc_hectares','ex_fs_date','ex_hectares']
df_adj = df[cols_adj].copy() #adjusted dataframe

#Discard NANs in fire type and fire fighting start date
df_adj.dropna(subset=['fire_type','fire_fighting_start_date'],inplace = True) 

#adjustments for the fire type variable
df_adj = df_adj.loc[df_adj['fire_type']!= '  '] # there is one missing value - discard it
df_adj['fire_type'] = df_adj['fire_type'].str.strip()

#adjustments for the fuel type
df_adj['fuel_type'].fillna('Other',inplace = True)  #replace nans with Other (unknown fuel type)

#create a response time variable (denoted in hours)
df_adj['response_time'] = (df_adj['fire_fighting_start_date'] - df_adj['reported_date']).dt.total_seconds() / 3600

#create a cause variable based on two causes: lightning and people
df_adj['cause'] = np.where(df_adj['general_cause_desc'] == 'Lightning','Lightning','People')
df_adj.drop(['general_cause_desc'],axis=1,inplace=True) #drop general cause variable

#group the fuel types for a simpler variable
fuel_dict = dict.fromkeys(['C1', 'C2', 'C3','C4', 'C6', 'C7'], 'Coniferous') | dict.fromkeys(['M1', 'M2','M3', 'M4'], 'Mixedwood') | \
            dict.fromkeys(['S1', 'S2'], 'Slash') | dict.fromkeys(['D1'], 'Leafless_Aspen') | \
            dict.fromkeys(['O1a', 'O1b',], 'Grass') | dict.fromkeys(['Other'], 'Other') 
df_adj['fuel_type_grouped'] = [fuel_dict[df_adj.loc[i,'fuel_type']] for i in df_adj.index]

#create a total duration variable (denoted in hours)
df_adj['total_duration'] = (df_adj['ex_fs_date'] - df_adj['reported_date']).dt.total_seconds() / 3600

#time-size dictionary for creation of cox data
time_cols = ['reported_date','assessment_datetime','fire_fighting_start_date','bh_fs_date', 'uc_fs_date','ex_fs_date']
size_cols = ['assessment_hectares','fire_fighting_start_size','bh_hectares','uc_hectares','ex_hectares']
time_size_dict = dict(zip(time_cols[1:],size_cols))

#discard wildfires where some time updates are recorded after having been extinguished
for i in time_cols[:-1]:
    df_adj = df_adj.loc[df_adj[i]<=df_adj[time_cols[-1]]]
    
#discard observations with zero total duration
df_adj = df_adj.loc[df_adj['total_duration'] != 0]

#create a log total duration variable for descriptive statistic and plots section
df_adj['log_total_duration'] = np.log(df_adj['total_duration'])

df_adj.reset_index(drop=True,inplace=True) #reset index
df_adj['fire_id'] = df_adj.index #create a fire id for the final data


'''
Descriptive Statistics and Preliminary Plots for the Total Duration
'''
#descriptive statistics
df_adj['total_duration'].describe().apply("{0:.2f}".format) 
df_adj[df_adj['cause'] == 'People']['total_duration'].describe().apply("{0:.2f}".format) 
df_adj[df_adj['cause'] == 'Lightning']['total_duration'].describe().apply("{0:.2f}".format) 

#histogram of total duration for people-caused fires
fig, axs = plt.subplots(1,2)
axs[0].hist(df_adj[(df_adj['cause'] == 'People')]['total_duration'],bins=15, edgecolor='k',fill=True)
axs[1].hist(df_adj[(df_adj['cause'] == 'People') & (df_adj['total_duration']<=720)]['log_total_duration'],bins=15,edgecolor='k',fill=True)
axs[0].set_xlabel('Total Duration (Hours)')
axs[0].set_ylabel('Count')
axs[1].set_xlabel('Total Duration (Log Hours)')
axs[1].set_ylabel('Count')
plt.suptitle('Histograms for People-Caused Wildfires')
fig.tight_layout()
plt.show()
fig.savefig('Images/Fig_People_Hist.eps',format = 'eps')

#histogram of total duration for lightning-caused fires
fig2, axs = plt.subplots(1,2)
axs[0].hist(df_adj[(df_adj['cause'] != 'People')]['total_duration'],bins=15, edgecolor='k',fill=True)
axs[1].hist(df_adj[(df_adj['cause'] != 'People') & (df_adj['total_duration']<=720)]['log_total_duration'],bins=15,edgecolor='k',fill=True)
axs[0].set_xlabel('Total Duration (Hours)')
axs[0].set_ylabel('Count')
axs[1].set_xlabel('Total Duration (Log Hours)')
axs[1].set_ylabel('Count')
plt.suptitle('Histograms for Lightning-Caused Wildfires')
fig2.tight_layout()
plt.show()
fig2.savefig('Images/Fig_Lightning_Hist.eps',format = 'eps')

#create pie charts for fire type and fuel type
fig3, axs = plt.subplots(1,2)
axs[0].pie(df_adj.groupby(['fire_type']).count()['fire_id'], labels=df_adj.groupby(['fire_type']).count().index, autopct='%1.1f%%')
axs[0].set_title('Fire Type')
axs[0].legend(bbox_to_anchor=(0.8,0),fontsize = 9)

axs[1].pie(df_adj.groupby(['fuel_type_grouped']).count()['fire_id'], labels=df_adj.groupby(['fuel_type_grouped']).count().index, autopct='%1.1f%%')
axs[1].set_title('Fuel Type Groups')
axs[1].legend(bbox_to_anchor=(1,0),fontsize = 9)
plt.show()
fig3.savefig('Images/Fig_Pie_Charts.eps',format = 'eps')

#create boxplots based on size classes
fig4, ax = plt.subplots()
sns.boxplot(data=df_adj, y='size_class', x='log_total_duration', order=['A','B','C','D','E'],showfliers=True)
ax.set_title('Boxplots of Duration Based on Size Class')
ax.set_ylabel('Size Class')
ax.set_xlabel('Total Duration (Log Hours)')
fig4.savefig('Images/Fig_bp_size_loghours.eps',format = 'eps')

#create boxplots on cause
fig5, ax =plt.subplots()
sns.boxplot(data=df_adj, x='cause', y='log_total_duration',showfliers=True)
ax.set_title('Boxplots of Duration Based on Cause')
ax.set_xlabel('Cause')
ax.set_ylabel('Total Duration (Log Hours)')
fig5.savefig('Images/Fig_bp_cause.eps',format = 'eps')

'''
End of Descriptive Statistics
'''

def get_sorted_dist_df(fire_loc, df_stations):
    '''
    This function sorts all the weather stations for each fire location based on proximity using the Euclidean distance
    Inputs: fire location as an np.array and weather stations dataframe
    Outputs: sorted weather stations dataframe
    '''
    df_stats_sorted = df_stations[['wmo','lat','lon']].copy()
    for j in df_stats_sorted.index:
        stat_loc = np.array((df_stats_sorted.loc[j,'lat'],df_stats_sorted.loc[j,'lon']))
        dist = np.linalg.norm(fire_loc - stat_loc)
        df_stats_sorted.loc[j,'dist'] = dist
    
    df_stats_sorted.sort_values('dist', ignore_index=True, inplace=True)
    
    return df_stats_sorted

'''
Incorporation of Fire Weather Index Variables
'''
#read data and preprocess for fire weather index variables
df_stations = pd.read_csv('Data/FWI/cwfis_allstn2022.csv') #weather stations data
df_stations['agency'] = df_stations['agency'].str.strip() #to be able to filter 'MSC   ' 
df_stations = df_stations[(df_stations['prov'] == 'AB')  & (df_stations['agency']=='MSC') & (df_stations['useindex']>=0)]
df_stations.reset_index(drop=True,inplace=True)

#read fwi variables data
df_fwi_2000s = pd.read_csv('Data/FWI/cwfis_fwi2000sv3.0opEC_2015.csv')
df_fwi_2010s = pd.read_csv('Data/FWI/cwfis_fwi2010sopEC.csv')
df_fwi = pd.concat([df_fwi_2000s,df_fwi_2010s],axis=0, ignore_index=True) #combine 2000s and 2010s dfs
df_fwi.drop(['aes','temp','td','rh','ws','wg','wdir','pres',
             'vis','precip','rndays','sog','opts'], axis=1, inplace=True) #drop redundant columns for faster processing

df_fwi['rep_date'] = pd.to_datetime(df_fwi['rep_date'], errors='coerce')
df_fwi =  df_fwi[(df_fwi['rep_date'].dt.year >= 2006) & (df_fwi['rep_date'].dt.date <= datetime.date(2019,3,5))] #further filter it based on the original daterange
df_fwi.reset_index(drop=True,inplace=True)
df_fwi['only_date']  = df_fwi['rep_date'].dt.date #faster processing with filtered dates beforehand


#define some variables for cox data creation
new_cols = ['ffmc','dmc','dc','bui','isi','fwi','dsr']
max_ws = 9 # use 10 closest weather stations
cols = ['fire_id','time_1','time_2','status','size','ffmc','dmc','dc','bui','isi','fwi','dsr']
df_cox = pd.DataFrame(columns=cols) 
cols_to_assign = ['fire_id']
cols_to_fill = ['fire_id','size'] 

#create a combined dataframe for the Cox model
start = time.time()
for i in df_adj.index:
    #create a df named df_times for each wildfire to append into the final cox data
    df_times = pd.DataFrame(columns= ['fire_id','time','size','ffmc','dmc','dc','bui','isi','fwi','dsr'])
    df_times.loc[:,'time'] = [df_adj.loc[i,j].to_pydatetime() for j in time_cols]
    df_times.loc[1:,'size'] = [df_adj.loc[i,time_size_dict[j]] for j in time_cols[1:]]
    df_times[cols_to_assign] = [df_adj.loc[i,j] for j in cols_to_assign]
    
    #expand df_times with the days that the fire continued
    start_date = df_adj.loc[i,'reported_date']
    end_date  = df_adj.loc[i,'ex_fs_date']
    day_dates = [start_date.date() + datetime.timedelta(days = j) for j in range((end_date.date() - start_date.date()).days)]
    
    for j in day_dates:
        if j not in df_times['time'].dt.date.unique():    
            new_row = pd.DataFrame([{'time': datetime.datetime.combine(j,datetime.time(12))}])
            df_times = pd.concat([df_times,new_row],axis=0, ignore_index=True)
    df_times.sort_values('time', ignore_index=True, inplace=True)
   
    #fill in missing values of size: first forward fill, then backward fill
    df_times[cols_to_fill] = df_times[cols_to_fill].fillna(method='ffill').fillna(method='bfill')
    df_times.loc[0,'size'] = np.nan

    #create duration column
    df_times.loc[0,'duration'] = 0
    df_times.loc[1:,'duration'] = [(df_times.loc[i,'time'] - df_times.loc[0,'time']).total_seconds() / 60 for i in df_times.index[1:]]
    df_times['only_date'] = df_times['time'].dt.date
    
    if df_times.loc[df_times.index[-1],'duration'] == 0: #to exclude fires that have zero duration
        continue
    
    #get the fire location and run the sorted weather stations function to obtain 10 closest weather stations
    fire_loc = np.array((df_adj.loc[i,'fire_location_latitude'],df_adj.loc[i,'fire_location_longitude']))
    df_stats_sorted = get_sorted_dist_df(fire_loc = fire_loc, df_stations = df_stations)
    ws_ordered = df_stats_sorted.loc[:max_ws,'wmo']
    
    #fill in df_times with fwi variables data from the closest weather stations.
    last_date = 0
    for j in df_times.index:
        if last_date == df_times.loc[j,'time'].date():
            df_times.loc[j, new_cols] = df_times.loc[j-1, new_cols]
        else:
            df_fwi_filtered = df_fwi.loc[df_fwi['wmo'].isin(ws_ordered)].copy()
            for k in ws_ordered:
                df_fwi_filtered = df_fwi_filtered.loc[(df_fwi_filtered['only_date']== df_times.loc[j,'only_date']) & (df_fwi_filtered['wmo'] == k)]
                df_fwi_filtered.reset_index(drop=True,inplace=True)
                if not df_fwi_filtered.empty:
                    if (df_fwi_filtered.loc[:,'calcstatus'] == 1).bool():
                        df_times.loc[j, new_cols] = df_fwi_filtered.loc[0,new_cols]
                        last_date = df_times.loc[j,'time'].date()
                        break
    
    #adjust the format of df_times to input Cox model
    df_times.drop_duplicates(subset=['size','ffmc','dmc','dc','bui','isi','fwi','dsr'], keep="last",inplace=True)
    df_times.drop_duplicates(subset=['duration'], keep='last',inplace=True) #drop same duration entries, keep last
    df_times.reset_index(drop=True,inplace=True)
    df_times[new_cols] = df_times[new_cols].fillna(method='ffill').fillna(method='bfill')
    
    #adjustments needed for Cox data preparation
    if not df_times[new_cols].isnull().any().any():
        df_times['time_1'] = df_times['duration'].shift(1)
        df_times.rename(columns={'duration': 'time_2', 'fire_location_latitude': 'latitude',
                                 'fire_location_longitude':'longitude'}, inplace=True)
        df_times.drop(['time','only_date'], axis=1, inplace=True)
        df_times['status'] = 0
        df_times.loc[df_times.index[-1],'status'] = 1
        df_times = df_times[cols]
        
        #append df_cox with df_times (formatted data for the Cox model)
        df_cox = pd.concat([df_cox,df_times[1:]],axis=0, ignore_index=True)
    
    #check the time every 100th iteration
    if (i+1) % 100 == 0:
        end = time.time()
        print(f'{i+1}th iteration completed with total of {(end - start):.2f} seconds or {((end - start)/60):.2f} minutes')



#convert times to hours
df_cox['time_1']=df_cox['time_1']/60 
df_cox['time_2']=df_cox['time_2']/60
#merge time-independent variables from df_adj
df_cox = pd.merge(df_cox,df_adj[['fire_id','fire_location_latitude','fire_location_longitude','fire_type','response_time','size_class','fuel_type_grouped','cause']],on='fire_id', how='left')
df_cox.rename(columns={'fire_location_latitude': 'latitude','fire_location_longitude':'longitude'}, inplace=True)

#write main cox
df_cox.to_csv('df_cox.csv',index = False)

#divide based on cause
df_cox_p = df_cox[df_cox['cause'] == 'People']
df_cox_l = df_cox[df_cox['cause'] == 'Lightning']

#write lightning- and people-caused fires dataframes to csv
df_cox_p.to_csv('df_cox_p.csv',index = False)
df_cox_l.to_csv('df_cox_l.csv',index = False)






