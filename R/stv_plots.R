# stv.plots - text of main statistics of election count

#' Makes webpage plots of result of an STV election
#'
#' @param elecdata An R list of results from stv or stv.wig
#' @param outdirec A directory for web page output
#' @param webdisplay If TRUE displays the main output web page
#' @param map A possible extra showing map of election location
#' @return Webpages with plots of election count and results
#' @export
#'
#' @examples c99result=stv(c99)
#' @examples stv.plots(c99result)
#' @examples nws17wig=stv.wig(nws17)
#' @examples stv.plots(nws17wig)

stv.plots=function(elecdata,outdirec=tempdir(),webdisplay=TRUE,map=FALSE){

if(!dir.exists(outdirec)){dir.create(outdirec)}

cd=elecdata; cdn=names(cd)

# vars from elecdata
elecname=cd$e
ns=cd$s
nc=cd$c
nv=cd$nv
fname=cd$f
name=cd$n
n2=cd$n2
party=cd$p
col=cd$col

# additional vars from count
sys=cd$sys
elec=cd$el
itt=cd$itt # - may not need 
ctext=cd$ctext
csum=cd$csum
qtext=cd$qtext # - may not need
va=cd$va
if(sys=="meek"){keep=cd$keep}

# if we have all those ...
nstages=dim(csum)[[2]]-1
qs=apply(csum[1:nc,1:nstages],2,sum)/(ns+1); qs=100*qs/nv

wi=(nc+4.5); w=wi*120   # plot width in (approx) inches, and in pixels
trf=c("","t")
for(ist in 1:nstages){
    vm=va[,,ist]
if(sys=="meek"){qpc=qs[[ist]]}else{qpc=qs[[1]]}
    it=itt[[ist]]
    dtext=ctext[,ist]                                     
for(i in 2:1){  # 2 plots, with/without separate transfers plot
    transf=i-1
    plotfile=paste(outdirec,paste0("stage",trf[[i]],ist,".jpg"),sep="/")
    h=600+200*transf
    grDevices::jpeg(plotfile,width=w,height=h)
#    cat("make_plots line 41",dim(vm),"\n\n")
    voteplot(ns,vm,qpc,it,dtext,n2,party,col,transf,elecname,sys)
    grDevices::dev.off()
}}
    
wp=webpages(elecdata,outdirec,map)
#  if(verbose==TRUE){grDevices::dev.off()}
 if(webdisplay==TRUE){utils::browseURL(wp[[1]],browser="open")}

}
