# to transfer severl ics files to one csv file
# Hong Li lihong2291@gmail.com
FilePath <- "/home/hongl/Downloads/";
ICSs <- list.files(FilePath, pattern = "^[calender]", full.names = TRUE);
nICS <- length(ICSs);
print(sprintf("in total there are %d events", nICS));
#CVSs <- matrix(nrow = nICS, ncol = 6);
#colnames(CVSs) <- c("
Subject <- NULL; StartDate <- NULL; StartTime <- NULL; EndDate <- NULL; EndTime <- NULL; Location <- NULL; Description <- NULL;
#for (iICS in seq(1, 1, 1)) {#nICS, 1)) {
for (iICS in seq(1, nICS, 1)) {
	print(sprintf("coverting event %d : %s", iICS, ICSs[iICS])); 
	Info <- readLines(ICSs[iICS], n = 12);
    Subject <- c(Subject, paste("B", unlist(strsplit(Info[11], "/"))[4], sep = ""));
	iDate <- unlist(strsplit(unlist(strsplit(Info[6], ":"))[2], "T"))[1];
	jDate <- sprintf("%s.%s.%s", substr(iDate, 7, 8), substr(iDate, 5, 6), substr(iDate, 1, 4));
	StartDate <- c(StartDate, jDate);
	iTime <- unlist(strsplit(unlist(strsplit(unlist(strsplit(Info[6], ":"))[2], "T"))[2], "Z"))[1];
    jTime <- sprintf("%s:%s", substr(iTime, 1, 2), substr(iTime, 3, 4));	
	StartTime <- c(StartTime, jTime);

    iDate <- unlist(strsplit(unlist(strsplit(Info[7], ":"))[2], "T"))[1];
	jDate <- sprintf("%s.%s.%s", substr(iDate, 7, 8), substr(iDate, 5, 6), substr(iDate, 1, 4));
	EndDate <- c(EndDate, jDate);
    iTime <- unlist(strsplit(unlist(strsplit(unlist(strsplit(Info[7], ":"))[2], "T"))[2], "Z"))[1];
	jTime <- sprintf("%s:%s", substr(iTime, 1, 2), substr(iTime, 3, 4));
	EndTime <- c(EndTime, jTime); 
    iDes <- unlist(strsplit(Info[11], "nnonsen"))[2]; 
#unlist(strsplit(unlist(strsplit(Info[10], ":"))[2], "\\\\")); jDes <- paste(iDes[1], iDes[2], sep = "");
	Description <- c(Description, iDes);
	#<- Info[10];
    iLoc <- unlist(strsplit(unlist(strsplit(Info[10], ":"))[2], "\\\\")); jLoc <- paste(iLoc[1], iLoc[2], sep = "");
    Location <- c(Location, jLoc);
}
CSVs <- cbind(Subject, StartDate, StartTime, EndDate, EndTime, Description, Location);
write.csv(CSVs, paste(FilePath, "Allcal.csv", sep =""), row.names = FALSE);
