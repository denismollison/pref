# functions transfer, keep and share; and decision; and select for permutations used in share
# keep calculates current vote totals vm from keep values 
# vm a matrix - row = 1st pref, col = who with currently
# case where equal prefs allowed
# -> delegates sharing out of each vote to function ..
# .. share which calculates details for a single vote
#

# apply(p17c$va,c(2,3),sum)

# transfer.R
# to transfer surpluses at each stage
transfer=function(k,qa,inn,iter,surplus,vote,mult,ns,ie,em,sel){
    nc=dim(vote)[[2]]; ic=1:nc
    while(surplus>em){
    vm=keep(k,ie,vote,mult,sel)
#   vmp=vm
    vc=apply(vm,2,sum)
    qa=sum(vc[1:nc])/(ns+1)   # revise quota
# calculate surplus of already-elected candidates
    inn=ic[ie==1]
    surplus=sum(vc[inn]-qa)
    k[inn]=k[inn]*qa/vc[inn] 
    iter=iter+1
}
d=list(k=k,vm=vm,vc=vc,qa=qa,inn=inn,iter=iter,sur=surplus)
}


keep=function(k,ie,vote,mult,sel){
nv=dim(vote)[[1]]; nc=dim(vote)[[2]]; ic=1:nc
vm=matrix(0,nrow=(nc+1),ncol=(nc+1))
#
# cycle looking at each vote
for(iv in 1:nv){
b=vote[iv,]
# awkward bit, splitting vote if more than one first pref
# not essential for calculating result, only for understanding transfers
# if(length(b[b!=0])==0){cat(iv,"\n")}
if(length(b[b!=0])==0){vm[(nc+1),(nc+1)]=vm[(nc+1),(nc+1)]+1}else{
f=min(b[b!=0])
e1=ic[b==f]
# if(length(e1)==nc){cat(iv,"\n")}
nf=length(e1)
for(j in e1){
    bj=b; bj[b>0]=b[b>0]+1
    bj[[j]]=1
# deal with this (part of a) vote, going through choices in order of pref
bj[ie==-1]=0     # ignore any preferences for excluded candidates
vm[j,]=vm[j,]+share(k,ie,bj,sel)*mult[[iv]]/nf   # for share see below
}}
}
vm
}


# share - function to share out one vote, b (in ballot format) - 30 Mar 2020
# when current keep values are k, and
# ie indicates candidate status = -1 if excl, 1 if elec, 0 if undecided
# need:  library(binaryLogic)
share=function(k,ie,b,sel){                                 #
nc=length(b); ic=1:nc
x=1   # = one single transferable vote
sh=rep(0,nc)   # for shares of vote
t=1-k
b[ie<0]=0   # treat excl candidates as though non-existent
if(max(b)>0){
    prefs=as.numeric(names(table(b[b!=0])))   # pref values used, in order
    mu=0   # no. of undecided in group
    ip=0
    while(mu==0 & ip<length(prefs)){
        ip=ip+1   # look at next set of equal prefs
        jp=ic[b==prefs[[ip]]]; m=length(jp)   # = me+mu
# separate into elec and undecided; elec trickier - they transfer surpluses
        je=ic[b==prefs[[ip]] & ie==1]; me=length(je)
        ju=ic[b==prefs[[ip]] & ie==0]; mu=length(ju)
        if(me==1){sh[je]=k[je]/m}
        if(me>1){
            z=sel[[me-1]]
            for(i in je){
                t=1-k[je[je!=i]]
                for(j in z){
                    j=as.logical(j)
                    shj=k[[i]]*factorial(sum(j))*prod(t[j])/prod(m:(m-sum(j)))
                    sh[[i]]=sh[[i]]+shj
                }}
        }
        if(mu>0){
            sh[ju]=(1-sum(sh[je]))/mu
        }
        sh[jp]=x*sh[jp]; x=x-sum(sh[jp])
    }
    }
c(sh,x)   # anything left over (x) is non-transferable
}


decision=function(nc,vc,qa,ie,surplus,k,em,stage,it,fin,vo,st){
#   to make next decision (elect/exclude)
  x=order(-vc[1:nc]);  x=x[ie[x]==0]; elec=numeric(); xcl=numeric()
  if(vc[x[[1]]]>=qa){
# if exact order of elec important, need to check margin > surplus 
# if margin <= surplus, need to reduce em and try again .. see spare.R#margin
    elec=x[vc[x]>=qa]
    surplus=surplus+sum(vc[elec])-qa*length(elec)
#    cat("  elected: ",name[itn]); cat("\n")
  }else{
# else exclude - first checking noone within em of quota
  if(vc[x[[1]]]<(qa-surplus)){
    x=order(vc[1:nc]);  xcl=x[ie[x]==0][[1]]   
    k[[xcl]]=0
    surplus=surplus+vc[[xcl]]
#    cat("  excluded: ",name[xcl]); cat("\n")
   }else{
   em=em*0.01	# should check for dead heat in case em->0?
   }
  }
if(length(c(elec,xcl))>0){
    ie[elec]=1; ie[xcl]=-1
    stage=stage+1
if(fin==0){
vo=cbind(vo,vc); st=c(st,paste("stage",stage,sep=""))
}}
dn=list(k=k,ie=ie,elec=elec,xcl=xcl,surplus=surplus,stage=stage,vo=vo,st=st)
}



decision_text=function(stage,ne,ns,elec,xcl,name,dnext){
#   function to generate output text for a stage
#   not expecting to elect and exclude at same stage
#   and only expect one exclusion at a time
if(ne==ns){final=" - final result"}else{final=""}
    dec2=""
if(stage==1){dec1=paste("first preferences - ",final,sep="")}else{
    dec1=paste("stage ",stage,final," - ",dnext,sep="")
 }
if(length(elec)>0){
    x=plural(name[elec])
    dec2=paste(x$out,x$has,"achieved the quota, so",x$is,"elected")# }
    dnext=paste("after transfer of surplus",x$es," of ",x$out,sep="")
}
if(length(elec)==0){
    if(ne==0){nomore="no-one"}else{nomore="no-one else"}
    dec2=paste(nomore,"has achieved the quota, so exclude",name[xcl])
    dnext=paste("after transfer of votes of",name[xcl])
}
list(t=c(dec1,dec2),d=dnext)
}


select=function(nmax){
# the 2^n ways of selecting a subset from a set of n, for up to n=nmax
# need for up to ns-1 for count, and ns for final keep value calculation
# only need to use once (`sel=select(ns)') in preamble
bi=list()
bi[[1]]=list(0,1)
for(n in 1:nmax){
bn=list()
for(j in 1:2^n){
bn[[j]]=c(0,bi[[n]][[j]])
bn[[2^n+j]]=c(1,bi[[n]][[j]])
}
bi[[n+1]]=bn
}
bi
}
