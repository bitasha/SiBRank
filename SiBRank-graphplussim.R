# This function provide the code to construct the SiBreNet graph and calculating users' similarity through SRank 
#rmat: rating matrix
#userseq: the array list in which i-th elements contain items rated by users
#testSet: the set of test users which we want to calculate their similarity ro other users.
#trList: The set of users who are used for training
#tr: the array list in which i-th elements contain items rated by users in training set
#damping factor: alpha parameter in SRank

SiBRankSimilarityBlock<-function(rmat,userSeq,testSet,tr,trList,damping){
  tt<-system.time(l<-make2LGraph_signed(rmat ,userSeq,testSet,tr));
  g<-graph.edgelist(l[,1:2],directed = F);
  g<-set.edge.attribute(g,"weight",1:ecount(g),l[,3]);
  g=g-setdiff(which(degree(g)==0),1:nrow(rmat))
  
  #calculate Similariity 
  tt<-system.time(Sim<-signedGraphSimilarity3(g,set1 = testSet,set2 = trList,rmat = rmat,damping = damping))[1]
  return(Sim)
}

#construct the SiBreNet 
#us: the array list in which i-th elements contain items rated by users
#testSet: the set of test users which we want to calculate their similarity ro other users.
#tr: the array list in which i-th elements contain items rated by users in training set

make2LGraph_signed<-function(rmat,us,testSet,tr){
  print("constructing graph")
  #l=matrix(0,ncol(rmat)^2+nrow(rmat),ncol(rmat)^2+nrow(rmat));
  l=rep(0,3);
  for(i in 1:nrow(rmat)){
    uid=i;
    if(any(testSet==i)){
      r=tr[[which(testSet==i)]];
    }
    else{
      r=which(!is.na(rmat[uid,]));
    }    
    s=splitRating(rmat,uid,r);
    count=0
    t2=rep(0,3);    
    if(length(s)>1){
    for(j in 1:(length(s)-1)){
      for(k in ((j+1):length(s))){
        if(length(s[[j]])>0&&length(s[[k]])>0){
          t=tList_signed(i,s[[j]],s[[k]],rmat)
          t2=rbind(t2,t);
          count=count+1;
        }
      }
    }
    }
    if(count>0){
      #  sam=sample(1:nrow(t2),5)
      l=rbind(l,t2[-1,]);
    }
    else{
      dd="(((("
    }
    # print(i)
    
  }
  l=l[-1,]
  
  return(l);
}

#make the a subgroup of edgelist
tList_signed<-function(i,s1,s2,rmat){
  m=matrix(0,length(s1)*length(s2),3) ;
  count=1; 
  for(j in 1:length(s1)){
  
    for(k in 1:length(s2)){
      if(s2[k]<s1[j]){
        m[count,]=c(i,hash_signed(s2[k],s1[j],nrow(rmat),ncol(rmat)),-1);
      }
      else{
        m[count,]=c(i,hash_signed(s2[k],s1[j],nrow(rmat),ncol(rmat)),+1);
      }
      count=count+1;
    }
  }
  return(m)
}

#calculating the id of preference node i1>i2
hash_signed<-function(i1,i2,nu,ni){
  n=ni;
  i=min(i1,i2);
  h=nu+(((n-1)*(n))/2)-((n-i+1)*(n-i)/2)+max(i1,i2)-i;
  return(h)
  
}

#calculating graph similarity through SRank 

signedGraphSimilarity3<-function(g,set1,set2,rmat,damping){
  isolate=which(degree(g)==0)
  isolate=setdiff(isolate,1:nrow(rmat));
  g=g-isolate;
  sim=matrix(0,length(set2),2*length(set1))
  for(i in 1:length(set1)){    
    print(i)
    l<-Signed_PageRank_L3(set1[i],g,damping);
    l=l[,set2]
    r=cbind(l[1,],l[2,]);#/(l2[1,]+l2[2,]);  
    sim[i,]=r;
    sim[i,i]=0;
    sim[i,i+nrow(rmat)]=0;
  }
  Sim=sim;
  nrs=nrow(sim)
  Sim=(Sim[,1:nrs]-Sim[,(nrs+1):ncol(Sim)])/(Sim[,1:nrs]+Sim[,(nrs+1):ncol(Sim)])
  for(jj in 1:ncol(Sim))
    Sim[jj,jj]=0
  
  print("similarity is calculated")
  return(Sim);
}

#Initializing parameters of SRank 
Signed_PageRank_L3<-function(id,g,damping=0.85){
  rp=rep(0,vcount(g));
  rn=rep(0,vcount(g));
  rp[id]=1;
  l2=Signedppr(g,rp,rn,damping);
  return(l2);
}


#SRank
Signedppr<-function(g, personalizedP=rep(1/vcount(g),vcount(g)),personalizedN=rep(0,vcount(g)),damping){
  r=get.adjacency(g,attr="weight");
  rp=r;
  rn=r;
  l=which(personalizedP!=0)  
  pp=which(r[l,]>0)
  pn=which(r[l,]<0)
#  l=which(personalizedP!=0)
  
 vv=personalizedP+personalizedN
 personalizedP=personalizedP/sum(vv)
personalizedN=personalizedN/sum(vv)
  
  fp=summary(rp);
  fn=summary(rn)  
  fp[which(fp[,3]==-1),3]=0;
  fn[which(fn[,3]==1),3]=0;
  fn[which(fn[,3]==-1),3]=1;
  rn=sparseMatrix(i=fn[,1],j=fn[,2],x=fn[,3]);
  rp=sparseMatrix(i=fp[,1],j=fp[,2],x=fp[,3]);
  
  e=drop0(r);
  
  ep=e;
  en=e
  fp=summary(en);
  fn=summary(ep)  
  fp[which(fp[,3]==-1),3]=0;
  fn[which(fn[,3]==1),3]=0;
  fn[which(fn[,3]==-1),3]=1;
  en=sparseMatrix(i=fn[,1],j=fn[,2],x=fn[,3],dims = c(vcount(g),vcount(g)));
  ep=sparseMatrix(i=fp[,1],j=fp[,2],x=fp[,3],dims = c(vcount(g),vcount(g)));
  ep=drop0(ep);
  en=drop0(en)
  p=colSums(abs(e))
  pp=colSums(e);
  pn=colSums(e);  
  #print(c(length(p),dim(ep)))
  ep=crossprod(ep,solve(Diagonal(x=p)))
  en=crossprod(en,solve(Diagonal(x=abs(p))))                              
  mp=runif(vcount(g),0,1);
  mn=runif(vcount(g),0,1);
  mp=rep(1,vcount(g));
  mn=rep(1,vcount(g))
  v=sum(mp+abs(mn));
  mp=mp/v
  mn=mn/v
  er=matrix(0,3,100)
  
  mp=matrix(mp,length(mp),1)
  mn=matrix(mn,length(mn),1);
  for(i in 1:20){  
    newmp=damping*(ep%*%mp+en%*%mn)+(1-damping)*personalizedP
    newmn=damping*(en%*%mp+ep%*%mn)+(1-damping)*personalizedN;    
    mp2=mp;
    mn2=mn;    
    mp=newmp;
    mn=newmn;
    v=sum(mp+abs(mn));
    er[1,i]=sum((mp-mp2)^2)
    er[2,i]=sum((mn-mn2)^2)
    #if(sum(er)<0.000001)
     # break;
  }
  #v2=((mp-mn)*(mp+mn)) 
  d=rbind(as.double(mp),as.double(mn))
  return(d);
}

#Estimate the preference matrix and make recommendation
rankList3<-function(sim,rmat,uid,tst,nl,trmat){
  f=makeAggMat3(sim,rmat,trmat,uid,tst,topK = nl);
  #print(uid)
  return(f);
}


makeAggMat3<-function(sim,rmat,trmat,id,tr,topK){
  dim(sim)
  x=rep(0,nrow(sim))
  y=rep(0,nrow(sim))
  s2=order(sim[id,],decreasing=T);
  #print("h")
  sim=as.numeric(sim[id,s2[1:topK]]);
  prefMat=matrix(0,ncol(rmat),ncol(rmat));
  si=matrix(0,ncol(rmat),ncol(rmat));
  for(k in 1:length(sim)){
    # print(c(id,k,s2[k]))
    
    # print(c(k,s2[k]))
    trs=splitRating(rmat = trmat,uid = s2[k],r=tr[[s2[k]]]);
    #print(calUserLength(trs))
    if(!is.na(sim[k])){    
      if(length(trs)>=2){
        
        for(i in 1:(length(trs)-1)){         
          aa=trs[[i]];
          for(j in (i+1):length(trs)){
            bb=trs[[j]];          
            for(a in 1:length(trs[[i]])){
              for(b in 1:length(trs[[j]])){    
                
                prefMat[aa[a],bb[b]]=prefMat[aa[a],bb[b]]+((sim[k])*(trmat[s2[k],bb[b]]-trmat[s2[k],aa[a]]));
                prefMat[bb[b],aa[a]]=prefMat[bb[b],aa[a]]-((sim[k])*(trmat[s2[k],bb[b]]-trmat[s2[k],aa[a]]));
                si[aa[a],bb[b]]=si[aa[a],bb[b]]+abs(c(sim[k]));
                si[bb[b],aa[a]]=  si[aa[a],bb[b]];
              }
            }
          }
        }              
      }
    }
  }
  notzero=which(si!=0)
  prefMat[notzero]=prefMat[notzero]/si[notzero]
  prefMat[notzero]=exp(prefMat[notzero])
  g<-graph.adjacency(prefMat,mode="directed",weighted=T);
  
  s=paste(paste("C:\\Users\\bita\\Documents\\Shams\\Sib(hist)-ML100K-0-",id,sep="-"),".csv",sep="");
  l=page.rank(g)$vector;
  return(l);
}


