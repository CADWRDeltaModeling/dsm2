BEGIN {
  model="h"			# h: hydro q: qual
  IGNORECASE=1
  FS="\t"
  OFS="\t"
}

model=="h" && /\tec\t/ {next}			# skip qual only output for Hydro
model=="q" && /\tstage\t/ {next}			# skip hydro only output for Qual
{
  chanlen = tolower($5)=="length" ? "length" : sprintf("%8d",$5)
  param[1]=$2
  if (model=="h" && param[1]=="all")
    param[1]="stage";param[2]="flow"
  if (model=="q" && param[1]=="all")
    param[1]="ec";delete param[2]
  for (p in param) {
    printf "%-17s %7d %8s %-8s %s %s %s\n",\
    tolower($3),$4,chanlen,param[p],"${FINE_OUT} inst      ${HYDROOUTDSSFILE}","#"$6,$8
  }
}
