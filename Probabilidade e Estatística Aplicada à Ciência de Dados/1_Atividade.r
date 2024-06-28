#####Carregando os dados####

####arquivo txt

dados=read.table("dados aids.txt",h=FALSE)
dados
attach(dados)
t=dados[,1]
censur=dados[,2]

dados=read.table("dados_dep.txt",h=TRUE)
dados
attach(dados)
require(survival)

####arquivo xlsx

require(readxl) ##ler arquivos do excel

dados=read_excel("dados_dep.xlsx",sheet="DepQui")
dados

dados=read_excel("dados_dep.xlsx",sheet="DadosAids")
dados

####Data frame: Essa estrutura de dados é uma espécie de tabela, de estrutura bidimensional de dados####

g<-data.frame('Marca'=c('Volkswagen','Fiat','Ford'),'Preço'= c(32000,28000,29500))
g

####Distribuição de frequências####

at<-c('C','L','L','M','C','M','So','L','L','C','M','C','S','L','C','LA',
'C','M','C','C')
tab.at<-table(at)
df<-matrix(0,5,3)
colnames(df)<-c("fa","fr","fp")
rownames(df)<-c("Café","Leite","Milho","Outras","Total")
df[1,1]<-tab.at["C"]
df[2,1]<-tab.at["L"]
df[3,1]<-tab.at["M"]
df[4,1]<-sum(tab.at["So"], tab.at["S"], tab.at["LA"])
df[5,1]<-length(at)
for(i in 1:5) {df[i,2]<-df[i,1]/length(at)}
for(i in 1:5) {df[i,3]<-df[i,2]*100}
df

####gráfico de barras####

gc<-barplot(df[1:4,2],xlab="Atividade",ylab="frequência relativa",col = gray(seq(0.4,1.0,length=4)))
gc

####gráfico de colunas####

gc<-barplot(df[1:4,2],horiz=TRUE,ylab="Atividade",xlab="frequência relativa",col=heat.colors(4))
gc

?barplot

####Setograma (gráfico de setores ou gráfico de pizza####

pie(df[1:4,2], col=heat.colors(4),radius=1.05)

coresBasicas <- c(1:4)
pie(df[1:4,2], col=coresBasicas,radius=1)

####distribuição de frequência do número de filhos por casal em um determinado município####

filhos<-c(3,4,3,1,3,2,1,1,2,2,4,4,1,3,2,2,4,4,3,3,1,0,2,1,3,2,2,4,
2,1,1,4,1,0,1,3,3,0,3,3)
tab.filhos<-table(filhos)
df<-matrix(0,6,3)
colnames(df)<-c("fa","fr","fp")
rownames(df)<-c(0,1,2,3,4,"Total")
df[1,1]<-tab.filhos["0"]
df[2,1]<-tab.filhos["1"]
df[3,1]<-tab.filhos["2"]
df[4,1]<-tab.filhos["3"]
df[5,1]<-tab.filhos["4"]
df[6,1]<-length(filhos)
for(i in 1:6) {df[i,2]<-df[i,1]/length(filhos)}
for(i in 1:6) {df[i,3]<-df[i,2]*100}
df

####Gráfico de colunas####

gb<-barplot(df[1:5,2], col=terrain.colors(5),xlab="Número de filhos", ylab="fr") 

####Distribuição de frequências - variáveis contínuas####

canela<-c(45.2,45.3,45.4,45.7,45.9,46.1,46.1,46.2,46.5,46.6,46.9,47.9,
48.1,48.1,48.3,48.5,48.8,48.8,49.1,49.2,49.3,49.7,49.8,49.9,50.1,50.2,
50.3,50.4,50.5,50.5,50.5,50.6,50.8,51.0,51.1,51.4,51.4,51.6,51.7,51.9,
52.5,52.7,52.8,53.0,54.9,55.0,55.2,55.3,55.7,55.7)
df<-matrix(0,8,3)
colnames(df)<-c("fa","fr","fp")
rownames(df)<-c('[44,33;46,08)','[46,08;47,83)','[47,83;49,58)',
'[49,58;51,33)','[51,33;53,08)','[53,08;54,83)','[54,83;56,58)','Total')
tab.canela<-table(cut(canela,breaks=c(44.33,46.08,47.83,49.58,51.33,53.08,
54.83,56.58)))
df[1:7,1]<-tab.canela
df[8,1]<-length(canela)
for(i in 1:8) {df[i,2]<-df[i,1]/length(canela)}
for(i in 1:8) {df[i,3]<-df[i,2]*100}
df

####Histograma do peso de potinhos de canela em pó em uma linha de produção####

h<-hist(canela, breaks=c(44.33,46.08,47.83,49.58,51.33,53.08,56.58),
freq=FALSE, ylab="Dfr", xlab="Canela em pó (g)", main="",
col=topo.colors(6))

####histograma com polígono de frequência####

h<-hist(canela, breaks=c(44.33,46.08,47.83,49.58,51.33,53.08,56.58),
freq=FALSE, ylab="Dfr", xlab="Canela em pó (g)", main="",
col=topo.colors(6))
points(h$mids, h$density, "l", lwd=2)

####Ogivas representando as frequências absolutas acumuladas acima de e abaixo de####

lim<-c(44.33,46.08,47.83,49.58,51.33,53.08,54.83,56.58)
ab<-c(0, 5, 11, 21, 35, 44, 44, 50)
ac<-c(50, 45, 39, 29, 15, 6, 6, 0)
plot(lim, ab, 'l', ylab='frequência acumulada', xlab='Canela em pó (g)')
points(lim, ac, "l")






