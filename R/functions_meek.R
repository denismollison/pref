# functions transfer, keep and share; decision and decision_text; and select for permutations used in share, plural for grammar
# keep calculates current vote totals vm from keep values 
# vm a matrix - row = 1st pref, col = who with currently
# case where equal prefs allowed
# -> delegates sharing out of each vote to function ..
# .. share which calculates details for a single vote


transfer=function(k,iter,vote,mult,ns,ie,em,surplus,sel){
# to transfer surpluses at each stage
nv=dim(vote)[[1]]; nc=dim(vote)[[2]]; ic=1:nc
    je=ic[ie==1]; ne=length(je)
v3=array(0,dim=c((nc+1),(nc+1),2^ne))
for(iv in 1:nv){
  b=vote[iv,]
# split vote if more than one first pref (needed to audit transfers)
  if(length(b[b!=0])==0){v3[(nc+1),(nc+1),1]=v3[(nc+1),(nc+1),1]+1  # (empty vote)
    }else{
  f=min(b[b!=0])
  e1=ic[b==f]   # could pick out "all equal" votes here (length(e1)==nc)
  nf=length(e1)
for(j in e1){
   bj=b; bj[b>0]=b[b>0]+1
   bj[[j]]=1
# deal with this (part of a) vote, going through choices in order of pref
   bj[ie==-1]=0     # ignore any preferences for excluded candidates
   v3[j,,]=v3[j,,]+share(ie,bj,sel)*mult[[iv]]/nf   # for share see below
}}
}
if(ne==0){iter=iter+1
    vm=v3[,,1]; vc=apply(vm,2,sum)
    qa=sum(vc[1:nc])/(ns+1)   # revise quota
}else{
    perm=sel[[ne]]
    v4=apply(v3,c(2,3),sum)
    i94=1
    while(surplus>em | i94==1){
    i94=0
    t=1-k; te=t[je]; tr=te[ne:1]
#   Calc vc from coefs using current val of t (=1-k)   
    coef=numeric()
    for(i in 1:2^ne){
      coef[[i]]=prod(tr^perm[[i]])
    }
    vc=v4%*%coef
    qa=sum(vc[1:nc])/(ns+1)   # revise quota
# calculate surplus of already-elected candidates
    surplus=sum(vc[je]-qa)
    k[je]=k[je]*qa/vc[je]; te=1-k[je]
    iter=iter+1
    }
    vm=matrix(0,nrow=(nc+1),ncol=(nc+1))
    for(i in 1:(nc+1)){
        for(j in 1:(nc+1)){
            vm[i,j]=sum(v3[i,j,]*coef)}}
}
list(k=k,vm=vm,vc=vc,qa=qa,inn=je,iter=iter,sur=surplus)
}


share=function(ie,b,sel){
# share - function to share out one vote, b (in ballot format)
# new version 10 Mar 2022 to save as polynomial coefficients
# ie indicates candidate status = -1 if excl, 1 if elec, 0 if undecided
nc=length(b); ic=1:nc
jj=ic[ie==1]; ne=length(jj)
code=rep(0,nc); code[jj]=2^(0:(ne-1))
sh=matrix(0,nrow=(nc+1),ncol=2^ne)   # for shares of vote
c0=1
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
        if(me==1){sh[je,c0+c(0,code[[je]])]=c(1,-1)/m}
        if(me>1){
            z=sel[[me-1]]
            for(i in je){
                c1=code[je[je!=i]]
                for(j in z){
                    j=as.logical(j)
                    amt=factorial(sum(j))/prod(m:(m-sum(j)))
                    coef=c0+sum(c1[j])+c(0,code[[i]])
                  sh[i,coef]=sh[i,coef]+amt*c(1,-1)
                }}
        }
        if(mu>0){
            d=rep(0,2^ne); d[[c0]]=1
            if(me==1){d=d-sh[je,]}
            if(me>1){d=d-apply(sh[je,],2,sum)}
            for(jj in 1:mu){sh[ju[jj],]=d/mu}
        }
        c0=c0+sum(code[je])
    }
    sh[(nc+1),c0]={mu==0}
    }else{sh[(nc+1),1]=1}
sh
}


decision=function(nc,vc,qa,ie,k,stage,fin,vo,st,surplus,hp){
#   to make next decision (elect/exclude)
    x=order(-vc[1:nc]);  x=x[ie[x]==0]; elec=numeric(); xcl=numeric(); hp0=hp
# will switch to `high precision' if a close call
# 1 is there another clearly elec?    
    if(vc[x[[1]]]>=qa){
# if exact order of elec important, need to check margin > surplus 
# if margin <= surplus, need to reduce em and try again .. see spare.R#margin
    elec=x[vc[x]>=qa]
    surplus=surplus+sum(vc[elec])-qa*length(elec)
  }else{
# 2 if at low precision, is there another within surplus of election?
if(hp==1 & vc[x[[1]]]>=(qa-surplus)){hp=2}else{
# 3 if at low precision
x=order(vc[1:nc]); x=x[ie[x]==0]
if(hp==1 & vc[[x[[1]]]]>(vc[[x[[2]]]]-surplus)){hp=2}else{
# 4 exclude lowest
xcl=x[[1]]   
k[[xcl]]=0
ic=1:nc
}
}}    
# skip output if have gone to high precn  (hp!=hp0)
if(hp==hp0){
    if(length(c(elec,xcl))>0){
    ie[elec]=1; ie[xcl]=-1
    stage=stage+1
if(fin==0){
vo=cbind(vo,vc); st=c(st,paste("st",stage,sep=""))
}}}
list(k=k,ie=ie,elec=elec,xcl=xcl,stage=stage,vo=vo,st=st,surplus=surplus,hp=hp)
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
# for use generating all subsets of a set of size n, for up to n=nmax
# returns a list whose nth element is the binary representations of 0:(2^n-1)
# needed for polynomial expression for transfer values
# need for up to ns-1 for count, and ns for final keep value calculation
# only need to use once (`sel=select(ns)') in preamble
bi=list()
bi[[1]]=list(0,1)
for(n in 1:(nmax-1)){
bn=list()
for(j in 1:2^n){
bn[[j]]=c(0,bi[[n]][[j]])
bn[[2^n+j]]=c(1,bi[[n]][[j]])
}
bi[[n+1]]=bn
}
bi
}


plural=function(names){
# function plural - for grammatical detail of output
n=length(names)
outnames=paste(names[[n]])
if(n>1){outnames=paste(names[[n-1]],"and",outnames)}
if(n>2){
  for(i in (n-2):1){outnames=paste(names[[i]],", ",outnames,sep="")}}
  if(n==1){is="is"; has="has"; es=""}else{is="are"; has="have";es="es"}
  list(out=outnames,is=is,has=has,es=es)
}
