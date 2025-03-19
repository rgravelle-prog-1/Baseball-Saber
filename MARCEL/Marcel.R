require(Lahman)
require(plyr)
data(People)  # Load People table (replaces Master)

pred.year = 2004

B = subset(Batting, yearID >= pred.year - 3 & yearID < pred.year)
B = transform(B, PA = AB + BB + HBP + SF + SH)

stats = c("PA", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO", "IBB", "HBP", "SH", "SF", "GIDP")
B = ddply(B[, c("playerID", "yearID", stats)], ~playerID + yearID, summarise,
          PA = sum(PA), AB = sum(AB), R = sum(R),
          H = sum(H), X2B = sum(X2B), X3B = sum(X3B), HR = sum(HR),
          RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB),
          SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH),
          SF = sum(SF), GIDP = sum(GIDP))

B$t = with(B, ifelse(pred.year - yearID == 1, 5, ifelse(pred.year - yearID == 2, 4, 3)))

P = subset(Pitching, yearID >= pred.year - 3 & yearID < pred.year)
B.pos = subset(merge(x=B, y=P[, c("playerID", "yearID", "BFP")], by=c("playerID", "yearID"), all.x=TRUE), PA > BFP | is.na(BFP))

p0 = ddply(B.pos, ~yearID, summarise, lgPA = sum(PA), lgAB = sum(AB)/sum(PA), lgR = sum(R)/sum(PA),
           lgH = sum(H)/sum(PA), lgX2B = sum(X2B)/sum(PA), lgX3B = sum(X3B)/sum(PA), lgHR = sum(HR)/sum(PA),
           lgRBI = sum(RBI)/sum(PA), lgSB = sum(SB)/sum(PA), lgCS = sum(CS)/sum(PA), lgBB = sum(BB)/sum(PA),
           lgSO = sum(SO)/sum(PA), lgIBB = sum(IBB)/sum(PA), lgHBP = sum(HBP)/sum(PA), lgSH = sum(SH)/sum(PA),
           lgSF = sum(SF)/sum(PA), lgGIDP = sum(GIDP)/sum(PA))

M = merge(x=B, y=p0, by="yearID")
stats.lg = paste("lg", stats, sep="")
X = M[, stats]
P0 = M[, stats.lg]
t.X = M$t * X
t.n = M$t * M$PA
t.n0 = M$t * 100
t.P0 = M$t * M$PA * P0
mPA = with(M, ifelse(pred.year - yearID == 1, 0.5 * PA, ifelse(pred.year - yearID == 2, 0.1 * PA, 200)))

Q = cbind(M[, c("playerID", "yearID")], t.n, t.X, t.n0, t.P0, mPA)
res = ddply(Q, ~playerID, summarise, numSeasons = length(t.n), reliability = sum(t.n)/(sum(t.n) + sum(t.n0)),
            tn = sum(t.n), PA = sum(PA), lgPA = sum(lgPA), mPA = sum(mPA),
            AB = sum(AB), lgAB = sum(lgAB), R = sum(R), lgR = sum(lgR),
            H = sum(H), lgH = sum(lgH), X2B = sum(X2B), lgX2B = sum(lgX2B),
            X3B = sum(X3B), lgX3B = sum(lgX3B), HR = sum(HR), lgHR = sum(lgHR),
            RBI = sum(RBI), lgRBI = sum(lgRBI), SB = sum(SB), lgSB = sum(lgSB),
            CS = sum(CS), lgCS = sum(lgCS), BB = sum(BB), lgBB = sum(lgBB),
            SO = sum(SO), lgSO = sum(lgSO), IBB = sum(IBB), lgIBB = sum(lgIBB),
            HBP = sum(HBP), lgHBP = sum(lgHBP), SH = sum(SH), lgSH = sum(lgSH),
            SF = sum(SF), lgSF = sum(lgSF), GIDP = sum(GIDP), lgGIDP = sum(lgGIDP))

stats.proj = setdiff(stats, "PA")
stats.m = paste("m", stats.proj, sep="")
stats.lg.proj = paste("lg", stats.proj, sep="")
res[, stats.m] = with(res, (reliability * res[, stats.proj]) / tn + (1 - reliability) * res[, stats.lg.proj] / tn)

res = merge(x=res, y=People[,c("playerID", "birthYear")], by="playerID")
res = transform(res, age = pred.year - birthYear)
res$age.adj = with(res, ifelse(age > 29, 0.003 * (age - 29), 0.006 * (age - 29)))
res[, stats.m] = res[, stats.m] * (1 + res$age.adj)
res[, stats.m] = res[, stats.m] * res$mPA

subset(res, playerID == "beltrca01", select=c("reliability", "age.adj", "tn", "mPA", "HR", "lgHR", "mHR"))

# Use local Marcel file
zipfile <- "F:/Baseball/MARCEL/marcel/marcel2004.zip"
dir.create("marcel", showWarnings = FALSE)
system(paste("unzip -n", shQuote(zipfile), "-d marcel"))
marcel = read.csv("marcel/BattingMarcel2004.csv")

test = merge(x=res, y=marcel, by="playerID")
subset(test, playerID == "beltrca01", select=c("age.adj", "reliability.x", "reliability.y", "mPA.x", "mPA.y", "mHR.x", "mHR.y"))

require(mosaic)
xyplot(reliability.x ~ reliability.y, data=test)
nrow(subset(test, abs(reliability.x - reliability.y) < 0.02)) / nrow(test)
nrow(subset(test, abs(mHR.x - mHR.y) < 0.5)) / nrow(test)

full = subset(test, numSeasons == 3)
with(full, cor(mPA.x, mPA.y))
with(full, cor(mHR.x, mHR.y))
xyplot(mHR.y ~ mHR.x, data=full)