letters
abc=sample(letters, size=10000, replace=T)
abc
for(i in length(abc))
if(abc[i]=="a"|abc[i]=="e"|abc[i]=="i"|abc[i]=="o"|abc[i]=="u"|abc[i]=="y")
{vowel=y+1}
"else{vowel+0}"
vowel