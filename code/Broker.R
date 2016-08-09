#rm(list = ls(all = TRUE))

# source('Broker3aPostgres.R')
setwd("C:/Users/Hamilton/_new/Ryerson/Data/TSX/R/Broker");


if( TRUE ){
	center = c();
	nbClusters = 5;
}

#generic info
broker_names <- read.csv("/Users/Hamilton/_new/Ryerson/Doc/Data/FileSpec/brokers.csv");
sector <- read.csv(paste(path, sep ="", 'sector.csv'),sep=',', header=T, stringsAsFactors=F)

	
nbBrokers = nrow(brokers);
outputBrokers = rep(0, nbBrokers);
outputBrokersName = rep('', nbBrokers);	
for(i in 1:nbBrokers){
	outputBrokersName[i] = paste( sep="-", brokers[i,1], brokers[i,2]);
}	
outputBrokersName2 = rep('', nbBrokers+5);	
outputBrokersName2[1:5] = c('ciclo1', 'ciclo2', 'ciclo3', 'ciclo4', 'ciclo5' );
outputBrokersName2[6:(nbBrokers+5)] = outputBrokersName;

sectorNames = rep('',nrow(sectors));
sectorNames[1] = 'broker';
for( i in 1:nrow(sectors) ){
	sectorNames[sectors[i,1]+2] = paste( sep="-", sectors[i,1], sectors[i,2]);
}
	
k=0;
dayNames = rep('',32);
output = matrix(0,nrow=nbBrokers,ncol=400);	
print( paste("nbBrokers=",nbBrokers) );
for(iMonth in 1:12){
    if( iMonth == 0 ) {
		iDiaLast = 0;
	} else {
		iDiaLast = 31;
	}
	if( TRUE ){
		#different centroides
		iDiaLast = 31;
		center = c();
	}
	
	for(iDia in 1:iDiaLast){
		print( paste("k: ", k, ", month: ", iMonth, ", day: ", iDia, " ") );
		print( paste(Sys.time()) ); 			
	
		output_sum = rep(0,nbBrokers);
		
		features = matrix(0,nrow=nbBrokers,ncol=(nrow(sectors)+1));
		dfRead = try( read.csv(  file = sprintf("output/file%02d%02d.csv", iMonth, iDia) ) );

		output_sum = as.character(dfRead$output_sum);
		nbActiveBrokers = sum(dfRead[,5:19]);
			
		features = as.matrix( dfRead[,4:(4+nrow(sectors))] );
		
		## don't like the center initial --> Erro: empty cluster: try a better set of initial centers
		if( nbActiveBrokers > 0 ){
			if( length(center) > 0 ){
				cluster2 <- try(kmeans(features[,2:(nrow(sectors)+1)], nstart = 3*nbClusters, centers=center));				
			} else {
				print("getting new center of cluster");
				cluster2 <- kmeans(features[,2:(nrow(sectors)+1)], nstart = 3*nbClusters, nbClusters);
				center = cluster2$center;
			}
			
			k=k+1;
			dayNames[k] = sprintf("%02d%02d", iMonth, iDia)

			if( class(cluster2) == "kmeans" ){
				output2 = matrix(0,nrow=nbBrokers+nbClusters,ncol=18);	
				output2[1:nbClusters,1] = 1:nbClusters;
				output2[1:nbClusters,2] = 0;
				output2[1:nbClusters,3] = 0;
				output2[1:nbClusters,4:18] = center[1:nbClusters,1:15];
				
				for( i in 1:nbClusters){
					n = length(which( cluster2$cluster == i ));
					outputBrokersName2[i] = paste(sep = "", "center", i, " (", n, ")");					
				}
								
				for(i in 1:nbBrokers){	
					n = which( features[,1] == brokers[i,1] );
					if( length(n) > 0 ){
						if( sum(features[n,2:(nrow(sectors)+1)]) < .1 ){
							output[i,k] = 0;
						} else {
							output[i,k] = cluster2$cluster[i];	
						}
					} else {
						output[i,k] = 0;
					}
					output2[i+5,1] = cluster2$cluster[i];					
					output2[i+5,2] = features[i,1];
					output2[i+5,4:18] = features[i,2:16];
				}			

				output_sum2 = rep('0', (nbBrokers+5));
				output_sum2[6:(nbBrokers+5)] = as.character( output_sum );
				#cluster / features
				dfOutput = data.frame(output2[,1:2], output_sum2, outputBrokersName2, output2[,4:18]);			
				names( dfOutput ) <- c("cluster","broker","vol", "name", sectorNames[2:16]);
				write.csv(  file = sprintf("output/Xfile%02d%02d.csv", iMonth, iDia), dfOutput );								
			} else {
				print("kmeans failed!");
			}
		}
	}
}
	
	

if( FALSE ){
	clusters = output[,1:k];
	dfTmp = data.frame(outputBrokersName,clusters);
	names(dfTmp) = c('broker', dayNames);
	write.csv(  file = "cluster.csv", dfTmp );
	
	output2 = matrix(0, nrow = nbBrokers, ncol=(nbBrokers+1) );	
	for(i in 1:nbBrokers){	
		#brokers[i,1]	
		x = clusters[i,];
		x[ which( x==0 ) ] = 100;
		output2[i,1] = length( which(x!=100) );
		
		for(j in 1:nbBrokers){
			y = clusters[j,];
			output2[i,j+1] = 1 - (length( which( (x-y)==0 ) )/output2[i,1]);
		}
		#output2[i,i+1] = 0;
	}
	
	
	dfTmp = data.frame(outputBrokersName,output2);
	names(dfTmp)[1:2]=c("brokers","days");
	names(dfTmp)[3:(nbBrokers+2)]=t(outputBrokersName);
	write.csv(  file = "output4.csv", dfTmp );
	
	
	#remove the broker with less than 100 days of trades.
	n = which( output2[,1] < 100 );
	output2Kmeans = output2[-n,-1];
	output2Kmeans= output2Kmeans[,-n];
	outputBrokersName2Kmeans = outputBrokersName[-n];	
	
	dfTmp = data.frame(outputBrokersName2Kmeans,output2Kmeans);
	names(dfTmp)[1]=c("brokers");
	names(dfTmp)[2:(ncol(output2Kmeans)+1)]=t(outputBrokersName2Kmeans);
	write.csv(  file = "output4.csv", dfTmp );	
	
	x = as.dist(output2Kmeans);
	hclust1 = hclust(x);
	hclust1$labels[1:length(outputBrokersName2Kmeans)] = substr(outputBrokersName2Kmeans, 1,10)
	plot(hclust1)
	
					
	

	output3 = matrix(' ', nrow = nbBrokers, ncol=(nbBrokers+1) );
	output3[,1] = as.character(output2[,1] );
	for(i in 1:nbBrokers){	
		cutNb = output2[i,1] * 0.9;
		n = which( output2[i,2:(nbBrokers+1)] >= cutNb );
		if( length(n) > 0 ){
			output3[i,2:(length(n)+1)] = outputBrokersName[n];
		}
	}
	dfTmp = data.frame(outputBrokersName,output3);
	names(dfTmp)[1:2]=c("brokers","days");
	write.csv(  file = "output5.csv", dfTmp );	

	
	x = output[,1:k];
	output2 = matrix(0,nrow=nbBrokers,ncol=6);
	dfOutput2 = data.frame(output2);
	names(dfOutput2) = c("no","1","2","3","4","5")

	output3 = rep(0,nbBrokers);
	for(i in 1:nbBrokers){
		y = table(x[i,])
		for( j in 1:length(y) ){
			tmp= as.numeric(names(y)[j])+1;
			dfOutput2[i,tmp] = y[j];
		}
		tmp = sort(dfOutput2[i,], decreasing = TRUE);
		output3[i] = names(tmp)[1];
	}

	w = data.frame( outputBrokersName, output3, dfOutput2 );
	write.csv(w, file='output2.csv');
}

