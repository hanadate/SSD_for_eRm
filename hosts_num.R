cores_num <- 
  function(hosts="hosts"){
    hosts_table <- read.table(hosts, header=FALSE)
    nCores <- sum(as.integer(gsub("cpu=","",hosts_table$V2)))
    return(nCores)
  }
hosts_num <- 
  function(hosts="hosts"){
    hosts_table <- read.table(hosts, header=FALSE)
    nHosts <- nrow(hosts_table)
    return(nHosts)
  }
