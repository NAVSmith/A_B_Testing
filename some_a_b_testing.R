

### loading the data set
df <- read.csv('./data/data_viz_2018.csv')

## inspect 

head(df)

df %>% select(condition) 

str(df$condition)

### 

df %>% count(condition)

count(df, condition)
print(count(df, condition))
## as we can see the dataset is balanced ofthe condition column 

## when condition is tips mean of time spent  
mean_time_spent_tips <- mean(df$time_spent_homepage_sec[df$condition == 'tips'])
print(mean_time_spent_tips)
## when condition is tips mean of time spent
mean_time_spent_tools <- mean(df$time_spent_homepage_sec[df$condition == 'tools'])
mean_time_spent_tools
### as we can see it seems there n 

t.test(df$time_spent_homepage_sec~df$condition)
## as we can see rthe diffrenace between the the wo group is not sig

## check the clicked_article column 

num_of_tips = sum(df$condition == 'tips')
num_of_tools = sum(df$condition == 'tools')
num_of_article_clicks_tips = sum(df$clicked_article[df$condition == 'tips'])
num_of_article_clicks_tools = sum(df$clicked_article[df$condition == 'tools'])

article_condition_matrix <- matrix(c(num_of_tips, num_of_tools,
                                  num_of_article_clicks_tips, num_of_article_clicks_tools),
                                  ncol=2)
colnames(article_condition_matrix) <- c('tips','tools')
rownames(article_condition_matrix) <- c('n_cases','n_clicks')
article_condition_matrix
result.prop <- prop.test(article_condition_matrix)
result.prop

### likes

num_of_like_clicks_tips = sum(df$clicked_like[df$condition == 'tips'])
num_of_like_clicks_tools = sum(df$clicked_like[df$condition == 'tools'])

like_condition_matrix <- matrix(c(num_of_tips, num_of_tools,
                                     num_of_like_clicks_tips, num_of_like_clicks_tools),
                                   ncol=2)
colnames(like_condition_matrix) <- c('tips','tools')
rownames(like_condition_matrix) <- c('n_cases','n_clicks')
like_condition_matrix
result1.prop <- prop.test(like_condition_matrix)
result1.prop


### share

num_of_share_clicks_tips = sum(df$clicked_share[df$condition == 'tips'])
num_of_share_clicks_tools = sum(df$clicked_share[df$condition == 'tools'])

share_condition_matrix <- matrix(c(num_of_tips, num_of_tools,
                                  num_of_share_clicks_tips, num_of_share_clicks_tools),
                                ncol=2)
colnames(share_condition_matrix) <- c('tips','tools')
rownames(share_condition_matrix) <- c('n_cases','n_clicks')
share_condition_matrix
result2.prop <- prop.test(share_condition_matrix)
result2.prop





t.test(df$clicked_article)