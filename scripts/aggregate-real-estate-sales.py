import pandas as pd 

raw = pd.read_csv('../raw/Real_Estate_Sales_2001-2016.csv', dtype=str)
town2fips = pd.read_csv('https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-county-fips-list/master/ct-town-county-fips-list.csv', dtype=str, index_col=0)

# We only care about single-family homes that are not classified by any Non-use code.
single_family = raw[
      (raw['ResidentialType'] == 'Single Family')
    & (raw['NonUseCode'].isna())
    & (~raw['SaleAmount'].isna())
]

def getBucket(p):
    try:
        p = int(p)
        if p < 100000:
            return 'Less than $100,000'
        if p < 200000:
            return '$100,000 to $199,999'
        if p < 300000:
            return '$200,000 to $299,999'
        if p < 400000:
            return '$300,000 to $399,999'
        return '$400,000 and Over'
    except:
        return 'Error converting price to bucket'

single_family['Bucket'] = single_family['SaleAmount'].apply(getBucket)

single_family['FIPS'] = single_family['Town'].apply(lambda t: town2fips.loc[t]['FIPS'])
single_family_counts = single_family.filter(['Town', 'ListYear', 'Bucket', 'ID']).groupby(['Town', 'ListYear', 'Bucket']).count()
single_family_counts['Measure Type'] = 'Number'
single_family_counts['Variable'] = 'Number of Home Sales'
#single_family_counts['FIPS'] = single_family_counts['Town'].apply(lambda t: town2fips.loc[t]['FIPS'])



single_family_counts.to_csv('../data/output.csv')