# output to top level web page
webpage_election=function(outdirec,election,mat_all,areas,areas2,pn,pna){
outdirec_html=paste(outdirec,"index.html",sep="/")
sink(outdirec_html)
cat(paste("<html>\n<head><title>",election,"</title>\n",sep=""))
cat(paste("<body>\n<h1>",election,"</h1>\n",sep=""))
cat("<b></b>")
# matrix of councillor numbers
tot0=apply(mat_all,2,sum)
mat=mat_all[,tot0>0,drop=FALSE]
tot=apply(mat,2,sum)
# put in order of numbers, except with Ind last ..
# .. followed by vacant (if any)
pm=dimnames(mat)[[2]]
p3=c(pm[pm=="Ind"],pm[pm=="vacant"])
p1=pm[pm!="Ind" & pm!="vacant"]
p1=p1[order(-tot[p1])]
p2=p1[tot[p1]<10]
p1=p1[tot[p1]>=10]
oth=sum(tot[p2])
names(oth)="Other"
y=c(tot[p1],oth,tot[p3])
pm=c(p1,p2,p3)
mat=mat[,pm]

cat("<p><em><b>Elected</b> - party totals:</em>","&#160;&#160;")
cat(paste(names(y),y,collapse=paste(", ","&#160;"))); cat("<p>")

matp=cbind(paste("<a href=\"",areas2,"/index.html\">",areas,"</a>",sep=""),mat)
matp=rbind(c("<em><b>Council</b> (click on name for details)</em>",dimnames(mat)[[2]]),matp)
aa=length(areas); pp=dim(matp)[[2]]
cat("<table cellpadding=3>")
for(i in 1:(aa+1)){
  cat("\n<tr>")
  for(j in 1:pp){
    cat("<td>",matp[i,j],"</td>",collapse="")
  }
  cat("</tr>")
}
cat("</table>")

cat("<p><b>Key to party acronyms:</b><p>")
cat("<b><em>Parties winning seats</em></b><p>")
cat("<table cellpadding=1>")
na=pm[pm!="vacant"]
for(i in 1:length(na)){
  cat("\n<tr><td>",na[[i]],"</td><td>","&#160;&#160;","</td><td>",pn[pna==na[[i]]],"</td></tr>")
}
cat("</table><br><br>")
j=1:length(pn)
io2=j[tot0[j]==0 & pn[j]!="vacant"]
cat("<b><em>Other parties contesting election</em></b><p>")
cat("<table cellpadding=1>")
for(i in io2){
cat("\n<tr><td>",pna[[i]],"</td><td>","&#160;&#160;","</td><td>",pn[[i]],"</td></tr>")
}
cat("</table>")
sink()
}


webpage_area=function(outarea,area,election,mat_area,wards2){
# for use with stv.batch
# does summary webpage at area (council) level
# 2022 from output_area.R, 2012/17
outarea_html=paste(outarea,"index.html",sep="/")
sink(outarea_html)
cat("<html>\n<head><title>",area, "</title>",sep="")
cat(paste0("\n<body><a href=../index.html><h3>",election,"</h3></a>"))
cat("\n<h2>",area,"</h2>",sep="")
# title, matrix of councillor numbers
area_total=apply(mat_area,2,sum)
    area_pos=area_total[area_total>0]
    pm=dimnames(mat_area)[[2]][area_total>0]
mat=as.matrix(mat_area[,area_total>0]); dimnames(mat)[[2]]=pm
# put parties in order of seats
    jp=1:length(pm)
    jj=jp[pm!="Ind" & pm!="vacant"]; jp[1:length(jj)]=order(-area_pos[jj])
    mat=mat[,jp]; area_pos=area_pos[jp]
cat("\n<p>\n<em><b>Elected</b> - party totals:</em>&#160;&#160;")
cat(paste(names(area_pos),area_pos,collapse=paste(", ","&#160;"))); cat("<p>")
matp=cbind(paste("<a href=\"",wards2,"/index.html\">",wards2,"</a>",sep=""),mat)
matp=rbind(c("<em><b>Ward</b> (click on name for details)</em>",dimnames(mat)[[2]]),matp)
ww=length(wards2); pp=dim(matp)[[2]]
# get rows in numeric order
    iw=numeric()
    for(i in 1:ww){
        iw[[i]]=as.numeric(strsplit(wards2[[i]],"_")[[1]][[1]])
    }
    ow=order(iw); ow=c(1,(ow+1))
    cat("<table cellpadding=3>")
for(i in 1:(ww+1)){
    cat("\n<tr>")
  for(j in 1:pp){
    cat("<td>",matp[ow[[i]],j],"</td>",collapse="")
  }
  cat("</tr>")
}
cat("</table>")
# with links down and up
sink()
}


nparty=function(py,pn){
  np=numeric()
  i=1:length(pn)
  n=length(py)
  for(j in 1:n){ 
    np[[j]]=i[pn==py[[j]]]
  }
  np
}
