BEGIN { 
}
{ if (NF < 3)
     print $0;
  else
     printf("%s 125 %d %s\n",$1,statenamemap[$3],statename[$3])
}
