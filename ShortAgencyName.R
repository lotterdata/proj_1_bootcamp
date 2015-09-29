Short.Agency.Names <- function(code){

	if(code == 1200) return('Agriculture')
	else if(code == 1300) return('Commerce')
	else if(code == 1400) return('Interior')
	else if(code == 1500) return('Justice')
	else if(code == 1600) return('Labor')
	else if(code == 1900) return('State')
	else if(code == 2000) return('Treasury')
	else if(code == 2400) return('Personnel Management')
	else if(code == 2800) return('Social Security')
	else if(code == 3100) return('Nuclear Reg Comm')
	else if(code == 3600) return('Veterans Affairs')
	else if(code == 4700) return('GSA')
	else if(code == 4900) return('NSF')
	else if(code == 6800) return('EPA')
	else if(code == 6900) return('Transportation')
	else if(code == 7000) return('Homeland Security')
	else if(code == 7200) return('USAID')
	else if(code == 7300) return('Small Business Admin')
	else if(code == 7500) return('HHS')
	else if(code == 8000) return('NASA')
	else if(code == 8600) return('HUD')
	else if(code == 8900) return('Energy')
	else if(code == 9100) return('Education')
	else if(code == 9700) return('Defense')
}

Vectorize(Short.Agency.Names)

temp <- read.csv("agency_codes.csv", header = TRUE)
for(code in temp$id){
  temp$ShortName[temp$id==code] <- Short.Agency.Names(code)
}
write.table(temp,"agency_codes.csv",row.names = FALSE, sep = ",")

rm(temp)
