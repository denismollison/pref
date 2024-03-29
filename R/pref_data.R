# main input function - last revised 30 jan 2024
# uses abbrev, party_colour and capwords (in functions_input.R) for tidying up
#
# handles vote data  with full details whether in pref or ballot format
# .. or in minimal format, a vote matrix with or without candidate names/codes
# outputs election as an R list, which can include: title, ns, nc
# candidate info: fname,name; and party
# full vote data matrix: vote, mult
# data in ballot format (if in pref order, use pref2blt.R to convert)

#' put election data in an R file (.rda)
#'
#' @param datafile File with election data
#' @param mult Whether includes aggregated votes (default FALSE)
#' @param parties File with party details (default FALSE, i.e. omit)
#' @param ballot Default FALSE (meaning pref format)
#' @param friendly Default FALSE (meaning most details after votes)
#' @param details Whether full election detail (default) or just vote matrix
#' @param header Whether a vote matrix has a header
#'
#' @return A standardised list of election info to save in a .rda file; for details see manual pref_pkg_manual.pdf (section 4)
#' @export
#' @examples
#' datafile=system.file("extdata","yale.dat",package="pref")
#' yale=pref.data(datafile,details=FALSE)
#' @examples
#' datafile=system.file("extdata","Jedburgh2012.blt",package="pref")
#' parties12=system.file("extdata","parties_SC2012.txt",package="pref")
#' jed12=pref.data(datafile,mult=TRUE,parties=parties12)
#' @examples
#' datafile=system.file("extdata","jmt2002.blt",package="pref")
#' j02=pref.data(datafile,friendly=TRUE)

pref.data=function(datafile,mult=FALSE,details=TRUE,parties=FALSE,ballot=FALSE,friendly=FALSE,header=FALSE){
# note: defaults are format used for (inter alia) Scottish Council elections,
# except that those data have mult=TRUE and parties=FALSE
    
# first: minimal case - data a vote matrix - if header=T with candidate names
if(details==FALSE){     # minimal case with abbrev names and vote matrix only
 vote=as.matrix(utils::read.table(datafile,header=header,row.names=NULL,sep=" "))
 vote[vote==""]=0
 if(mult==TRUE){mul=vote[,1]; vote=vote[,2:dim(vote)[[2]]]}else{
  mul=rep(1,dim(vote)[[1]])}
 nv=dim(vote)[[1]]; nc=dim(vote)[[2]]
 if(header==TRUE){name2=dimnames(vote)[[2]]}else{name2=let(nc)}
 fname=rep("",nc); name=name2; party=rep("",nc)
 colour=grDevices::rainbow(nc)
 elecname = readline("election name?")
 ns = readline("number to elect?"); if(is.na(ns)){ns=4}else{ns=as.numeric(ns)}
 if(elecname==""){elecname="Election data"}
 if(is.na(ns) | ns==0){ns=4}
}else{
# detailed case: details=T - elecname, ns, nc, names - option of parties
 dat=base::readLines(datafile)
 id=1:length(dat)
 name=character(); fname=character(); mul=numeric(); vote=numeric()
 if(friendly==TRUE){    # user-friendly file order, with details first, then votes
  elecname=dat[[1]]
  x=as.numeric(strsplit(dat[[2]]," ")[[1]])
  ns=x[[1]]; nc=x[[2]]; ic=1:nc
  cdata=2+ic
# what if no contest, therefore no vote data? (need to cover this case for batch mode)
  if(ballot==TRUE){nv=length(dat)-(nc+2)}else{nv=length(dat)-(nc+3)}
  if(nv>0){vdata=(nc+3):length(dat)}
 }else{
# if friendly=FALSE, i.e. data in user-unfriendly file order, with most details at end (Hill's format)
  x=as.numeric(strsplit(dat[[1]]," ")[[1]])
  nc=x[[1]]; ns=x[[2]]; ic=1:nc
  nv=id[substring(dat[id],1,1)=="0"]-2
  if(nv>0){vdata=2:(nv+1)}
  cdata=(nv+3):(nv+nc+2)
  elecname=dat[[nv+nc+3]]
 }
# for either input order, can now extract details and votes
# candidate names, calculate a short-form unique version, name2
 party=rep("",nc); fname=rep("",nc); name=rep("",nc)
 for(i in cdata){
  j=i+1-cdata[[1]]
  x=strsplit(dat[[i]],",")[[1]]
  if(length(x)>1){
   party[[j]]=trimws(x[[2]])
  }
  y=strsplit(x[[1]]," ")[[1]]; z=length(y)
  name[[j]]=y[[z]]
  if(z>1){fname[[j]]=paste(y[1:(z-1)],collapse=" ")}
 }
 name2=abbrev(name,fname)
# parties and party colours if specified
 if(parties!=F){
  ip=ic[party!=""]
  if(length(ip)>0){
   colour=rep("white",nc)
   colour[ip]=party_colour(party[ip],parties)
  }else{warning("recommend re-run with party colours file if available")}
 }else{colour=grDevices::rainbow(nc)}
# and last but not least - the votes
 if(nv>0){
  vote=matrix(0,nrow=nv,ncol=nc)
  if(mult==F){mul=rep(1,nv)}
  if(ballot==T){
   for(iv in 1:nv){
    x=strsplit(dat[[vdata[[iv]]]]," ")[[1]]
    if(mult==T){mul[[iv]]=as.numeric(x[[1]]); x=x[2:length(x)]}
    if(length(x)!=nc){warning("vote ",i," has length ",length(x))}
    vote[iv,]=as.numeric(x)
  }}else{
# if ballot=F
   for(iv in 1:nv){
    x=strsplit(dat[[vdata[[iv]]]]," ")[[1]]
    x=x[x!=""]
    if(mult==TRUE){mul[[iv]]=as.numeric(x[[1]])}else{mul[[iv]]=1}
    nx1=1+as.numeric(mult)
    nx2=length(x)
    if(x[[nx2]]!="0"){warning("check failure at vote no. ",iv,"\n")}
    if(nx2>nx1){
     x=x[nx1:(nx2-1)]; nx=length(x)   # actual vote
     pr=1; incr=1; pref=numeric()
     for(i in 1:nx){
      nch=nchar(x[[i]])
      if(substring(x[[i]],1,1)=="("){incr=0; x[[i]]=substring(x[[i]],2,nch)}
      if(substring(x[[i]],nch,nch)==")"){incr=1; x[[i]]=substring(x[[i]],1,(nch-1))}
      pref[[i]]=pr; pr=pr+incr
     }
     xn=as.numeric(x)
     vote[iv,xn]=pref
    }
  }}
 }else{mul=numeric(); vote=numeric()}   # uncontested case, recognised by nv=0
}
list(e=elecname,s=ns,c=nc,nv=nv,m=mul,v=vote,f=fname,n=name,n2=name2,p=party,col=colour)
}
# end of function prefdata; may want to save result, d say, using`save(d,file="elec.rda")'
# .. and later `load(d,file="elec.rda")'
