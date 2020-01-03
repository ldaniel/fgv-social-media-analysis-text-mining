
# sync with kaggle code.
# the objective of this step is to sync up the owrk developed in kaggle with this git reposirtory.

k_addess = 'rodrigonca/lyrics-text-mining'
k_url = paste('https://www.kaggle.com/', k_addess, sep = '')

# download kaggle notebook R code as R scripit.
writeLines(rawr::kaggle(k_url), 'src/playground/playground_lyrics_kaggle.R')

# download kaggle notebook
# to make it work you will need to install kaggle public API and set up your API credentials.
# https://github.com/Kaggle/kaggle-api#api-credentials
# make sure to save your API credentials at: C:\Users\<username>\Documents\.kaggle\kaggle.json

system(paste('kaggle kernels pull ', k_addess, ' -p "',  getwd(), '/src/playground/"', sep = ''))

# download kaggle notebook outputs
system(paste('kaggle kernels output ', k_addess, ' -p "', getwd() , '/data/processed/kaggle/"', sep = ''))

rm(k_addess, k_url)
