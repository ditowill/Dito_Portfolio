library(arules)

#Mengambil statistik produk 10 teratas (paling laku)

transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
transaksi_tabular<- sort(itemFrequency(transaksi_tabular,type ="absolute"), decreasing = TRUE)
transaksi_tabular<- transaksi_tabular[1:10]
transaksi_tabular <- data.frame("Nama Produk"=names(transaksi_tabular), "Jumlah"=transaksi_tabular, row.names=NULL)

print(transaksi_tabular)
write.csv(transaksi_tabular, file="top10_item_retail.txt")


#Mengambil statistik produk 10 terbawah (paling tidak laku)

transaksi_tabular<- sort(itemFrequency(transaksi_tabular,type ="absolute"), decreasing = FALSE)
transaksi_tabular<- transaksi_tabular[1:10]
transaksi_tabular <- data.frame("Nama Produk"=names(transaksi_tabular), "Jumlah"=transaksi_tabular, row.names=NULL)

print(transaksi_tabular)
write.csv(transaksi_tabular, file="bottom10_item_retail.txt")


#Mendapatkan kombinasi produk yang menarik
transaksi <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)

mba <- apriori(transaksi, parameter = list(support =10/length(transaksi), confidence =.5, minlen=2,maxlen=3))
mba<- sort(mba, by= "lift")
mba<-mba[1:10]
inspect(mba)

write(mba, file="kombinasi_retail.txt")


#Mencari Paket Produk yang bisa dipasangkan dengan Item Slow-Moving

a <- subset(mba, rhs %in% "Tas Makeup")
a <- head(sort(a,by="lift",decreasing = TRUE),3)
b <- subset(mba,rhs %in% "Baju Renang Pria Anak-anak")
b <- head(sort(b,by="lift",decreasing = TRUE),3)
c <- c(a,b)

inspect(c)

write(c, file="kombinasi_retail_slow_moving.txt")

