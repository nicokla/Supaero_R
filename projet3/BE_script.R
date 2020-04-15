#install.packages('MDPtoolbox')
library(MDPtoolbox)

h<-function(x)
{
  return(x)
}

g<-function(x)
{
  return((4+2*x)*(x>0))
}

f<-function(x)
{
  return(8*x)
}

p=c(1/4,1/2,1/4)
M=3
discount=0.99

# matrice de transition
P1=list();
for (i in 0:M){
  P1[[i+1]]=matrix(rep(0,time=(M+1)^2),ncol=M+1)
}

for (i in 0:M){
  for (j in 0:M){
    for (c in 0:M){
      for (d in 0:2){
        if(max(min(i+c,M)-d,0)==j){
          P1[[c+1]][i+1,j+1]=P1[[c+1]][i+1,j+1]+p[d+1]
        }
      }
    }
  } 
}

# matrice de gain
R1=matrix(rep(0,time=(M+1)^2),ncol=M+1);

for (s in 0:M){
  for (c in 0:M){
    for (d in 0:(M-1)){
      R1[s+1,c+1]=R1[s+1,c+1]+p[d+1]*f(min(d,s+c,M));
    }
    R1[s+1,c+1]=R1[s+1,c+1]- g(c) - h(min(s+c,M));
  }
}

#*************************** Question b
Optimal_policy=list()
Nombre_periode=c(1,2,3,5,10)
n=length(Nombre_periode)
for ( i in 1:n){
  Optimal_policy[[i]]=mdp_finite_horizon(P1, R1, discount, Nombre_periode[i])$policy;
}

#**************************** Question c
nn=mdp_policy_iteration(P1, R1, discount);
politique_optimale=nn$policy

#*************************** Question d: 
cc=mdp_computePpolicyPRpolicy(P1, R1, politique_optimale);
Mat_ChaineMarkov=cc[[1]]

# Distribution stationnaire
vec=c(1,1,1,1)
mat=t(Mat_ChaineMarkov)-diag(vec)
mat[4,]=vec
vec.y=c(0,0,0,1)
prob_stat=solve(mat)%*%vec.y

# récompense moyenne sur une seule période
recompense_Periode=cc[[2]]
recompense_moyenne=recompense_Periode%*%prob_stat

# Stock asymptotique
stock_Asymptotique=t(prob_stat)%*%c(0,1,2,3)


#************************ Question e: influence de discount





#*****************************************************************
#***************        Partie deux
#*****************************************************************

# Matrice de transition
P2=list();
P2[[1]]=matrix(rep(c(0.25, 0.5, 0.25, 0),4),nc=4,byrow=T);
# Matice de gain
R2=as.matrix(c(0,1,6,11));

#*************************** Question b
nn2=mdp_policy_iteration(P2, R2, discount);
politique_optimale2=nn2$policy
cc2=mdp_computePpolicyPRpolicy(P2, R2, politique_optimale2);

# prob_stat2 = c(0.25, 0.5, 0.25, 0)
# 
# # récompense moyenne sur une seule période
# recompense_Periode2=cc2[[2]]
# recompense_moyenne2=recompense_Periode2%*%prob_stat2
# 
# # Stock asymptotique
# stock_Asymptotique2=t(prob_stat2)%*%c(0,1,2,3)


#**************************** Question c

# matrice de transition
# P3=list();
# for (i in 0:3){
#   P3[[i+1]]=matrix(rep(0,time=12^2),ncol=12)
# }
# 
# for (c in 0:3){
#   for (s in 0:3){
#     for (d in 0:2){
#       for (s2 in 0:3){
#         for (d2 in 0:2){
#           if(max(min(s+c,M)-d,0)==s2){
#             P3[[c+1]][3*s+d+1,3*s2+d2+1]=
#               P3[[c+1]][3*s+d+1,3*s2+d2+1]
#                +p[d2+1]
#           }
#         }
#       }
#     } 
#   }
# }
# 
# # matrice de gain
# R3=list();
# for (i in 0:3){
#   R3[[i+1]]=matrix(rep(0,time=12^2),ncol=12)
# }
# 
# for (c in 0:3){
#   for (s in 0:3){
#     for (d in 0:2){
#       for (s2 in 0:3){
#         for (d2 in 0:2){
#           if(max(min(s+c,M)-d,0)==s2){
#             R3[[c+1]][3*s+d+1,3*s2+d2+1]=
#               R3[[c+1]][3*s+d+1,3*s2+d2+1]
#               + p[d2+1]*f(min(d2,s+c,M))
#           }
#         }
#         R3[[c+1]][3*s+d+1,3*s2+d2+1]=R3[[c+1]][3*s+d+1,3*s2+d2+1]- g(c) - h(min(s+c,M));
#       }
#     } 
#   }
# }
# nn3=mdp_policy_iteration(P3, R3, discount);
# politique_optimale=nn$policy

#*************************** Question d: 


#************************ Question e: influence de discount







