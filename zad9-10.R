help_Spirmen=function(table,ind)
{
  table=table[order(table[,ind]),]
  table[,ind+2]=1:nrow(table)
  line=table[,ind]
  for (i in as.numeric(names(table(line)[table(line)>1]))){
    sum=0
    for (j in which(line %in% i))
    {
      sum=sum+table[j,ind+2]
    }
    sum=sum/length(which(line %in% i))
    table[which(line %in% i),ind+2]=sum
  }
  return(table)
}
Spirmen=function(x1,x2)
{
  if(length(x1)!=length(x2))
  {
    return("Problem with the lengths of the vectors")
  }
  table=matrix(ncol=5,nrow=length(x1))
  table[,1]=x1
  table[,2]=x2
  table=help_Spirmen(table,1)
  table=help_Spirmen(table,2)
  table[,5]=(table[,3]-table[,4])**2
  sum=sum(table[,5])
  result=1-6*sum/(length(x1)*(length(x1)**2-1))
  return(result)  
}
med=function(elements)
{
  elements=elements[order(elements)]
  if(length(elements)%%2==0)
  {
    n=length(elements)%/%2
    element=(elements[n]+elements[n+1])/2
  }
  else
  {
    element=elements[length(elements)%/%2+1]
  }
  return(element)
}

