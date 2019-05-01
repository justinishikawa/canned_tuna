---
author: "Justin Ishikawa"
output: html_document
---

```{r, echo=TRUE}
library(tidyverse)
library(bayesm)
data(tuna)
#Train Test Split
set.seed(3)
Data.Indexes <-sample(1:nrow(tuna), size=.632*nrow(tuna))
Train<-tuna[Data.Indexes,]
Test<-tuna[-Data.Indexes,]

```

Business Case:  I'm choosing to look at price elasticities of sales due to the limitations of the data only having 7 SKUs thus giving us limited information about market share


Star Kist 6 oz.

```{r, echo=TRUE}
starkist<-lm(log(MOVE1)~LPRICE1+LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = Train)
summary(starkist)
#Validation r^2
starkist_test_results<-predict(starkist, Test)
cor(log(Test$MOVE1),starkist_test_results)^2
plot(starkist$residuals)
```

We see from the summary above Star Kist is highly sensitive to changes in its price which is not surprising for a market leader. We also recognize that Star Kist is sensitive to price changes in Bumble Bee Chunk and Chicken of the Sea. This model explains 61% of variance and validation r squared for this model was 47%



Chicken of the Sea 6 oz.

```{r, echo=TRUE}
chicken<-lm(log(MOVE2)~LPRICE1+LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = Train)
summary(chicken)

#Validation r^2
chicken_test_results<-predict(chicken, Test)
cor(log(Test$MOVE2),chicken_test_results)^2
plot(chicken$residuals)

```

Chicken of the Sea is sensitive to price changes in the price of itself, Star Kist, and Bumble Bee Chunk. Since these three products were also highly connected in the model for StarKist, this leads me to have the hypothesis that the consumer finds these three brands substitutes for each other which leads to constant competition between them. This model explains 56% of the variance and the r squared for the validation set was 71%



Bumble Bee Solid 6.12 oz.

```{r, echo=TRUE}
BB_solid<-lm(log(MOVE3)~LPRICE1+LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = Train)
summary(BB_solid)

#Validation r^2
BB_solid_test_results<-predict(BB_solid, Test)
cor(log(Test$MOVE3),BB_solid_test_results)^2
#plot of residuals
plot(BB_solid$residuals)
```

Bumble Bee Solid is an interesting product. As expected we see this product highly sensitive to price changes in itself but also price changes in Bumble Bee Chunk, Geisha and HH Chunk Lite. This model explains 17% of the variance. Validation R squared is 22%



Bumble Bee Chunk 6.12 oz.

```{r, echo=TRUE}
BB_chunk<-lm(log(MOVE4)~LPRICE1+LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = Train)
summary(BB_chunk)

#Validation r^2
BB_chunk_test_results<-predict(BB_chunk, Test)
cor(log(Test$MOVE4),BB_chunk_test_results)^2
#plot of residuals
plot(BB_chunk$residuals)
```

Bumble Bee Chunk is highly depended on changes in its price and the prices of Star Kist and Chicken of the Sea. As we discussed in the models for Star Kist and chicken of the sea, it appears these products are highly substitutable in the minds of the consumer. 70% of the variance is explained by this model and validation r squared is 55%

Geisha 6 oz.

```{r, echo=TRUE}
geisha<-lm(log(MOVE5)~LPRICE1+LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = Train)
summary(geisha)

#Validation r^2
geisha_test_results<-predict(geisha, Test)
cor(log(Test$MOVE5),geisha_test_results)^2
#plot of residuals
plot(geisha$residuals)
```

Geisha is also an interesting brand. It seems only highly dependent on changes in its price; this leads me to believe that this is either a niche product or a product that is considered a value brand where consumers are choosing to purchase this product for reasons unrelated to prices of better known, more popular products. This model explains 49% of variance and validation r squared is 58%

BB Large Cans.

```{r, echo=TRUE}
BB_large<-lm(log(MOVE6)~LPRICE1+LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = Train)
summary(BB_large)

#Validation r^2
BB_large_test_results<-predict(BB_large, Test)
cor(log(Test$MOVE6),BB_large_test_results)^2
```

Bumble Bee Large Cans appears to be in a category into itself. Its volumes are not sensitive to prices of itself and barely sensitive to the price of the market leader, star kist. I would hypothesize that due to the size being larger than the other products in the dataset, the consumers purchasing this need the size for convienence possibly for food service and savings due to small fluctuations in price of smaller formats are outweighed by the time it would take to open multiple cans of an equivalent quantity.

HH Chunk Lite 6.5oz.

```{r, echo=TRUE}
HH_chunk<-lm(log(MOVE7)~LPRICE1+LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = Train)
summary(HH_chunk)

#Validation r^2
HH_chunk_test_results<-predict(HH_chunk, Test)
cor(log(Test$MOVE7),HH_chunk_test_results)^2
#plot of residuals
plot(HH_chunk$residuals)
```

HH Chunk Lite seems highly dependent on the price of itself and the price of the Bumble Bee Large Cans and Star Kist. The model explains 29% of the variance and the validation r squared is 22%

Limitations of models: Due to the data provided to us we are only able to look at the effect on sales due to price, but in the real world there would be multiple factors that would go into the calculus regarding a change in sales amount. Some things would be advertising, promotions, availability on shelves, and numerous other factors.
