#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
typedef struct pivo
{
	int linha;
	int coluna;
} Pivo;
long double** alocarMatriz(int Linhas,int Colunas)
{ //Recebe a quantidade de Linhas e Colunas como Parâmetro
  int i=0,j=0; //Variáveis Auxiliares
  long double **m = (long double**)malloc(Linhas * sizeof(long double*)); //Aloca um Vetor de Ponteiros
  for (i = 0; i < Linhas; i++)
  { //Percorre as linhas do Vetor de Ponteiros
       m[i] = (long double*) malloc(Colunas * sizeof(long double));
       for(j=0; j< Colunas;j++)
			 {
         m[i][j]=0;
			 }
  }

  return m; //Retorna o Ponteiro para a Matriz Alocada
}
void desalocarMatriz(int Linhas, long double ** m)
{
  int i=0;
  for (i=0; i< Linhas; i++)
  {
    free(m[i]);
  }
}
void printarMatriz(long double** m,int linhas, int colunas)
{
  int i=0;
  int j=0;
	FILE *fp;
	if((fp = fopen("matrizes.txt", "a+")) == NULL)
	{
		perror("Erro ao abrir solucao");
		exit(1);
	}

  for (i=0; i< linhas;i++)
  {
    for(j=0; j<colunas;j++)
    {
			fprintf(fp, "%Lf \t", m[i][j]);
			printf("%.3Lf \t", m[i][j]);

    }
		fprintf(fp, "\n");
		printf("\n");
  }
	fprintf(fp, "\n");
	printf("\n");
	fclose(fp);
}
//função busca menor elemento para poder pivotear
int minElemento(int tamanho, long double *v)
{
  int i;
  long double menor = v[0];
  int posicao =0;
  int j=0;
  //definindo que o critério é o número de maior módulo
  for (i=0; i< tamanho; i++)
  {
    if(v[i] < menor && v[i] < 0)
    {
      posicao =i;
      menor = v[i];
    }
  }
  return posicao;
}
void pivotear(long double** m,int l,int c, int linhas, int colunas,long double** certificado)
{
  int i=0;
  int j=0;
  long double divisor =m[l][c];
  //divide a linha pelo índice desejado, ou seja transformei em 1 o elemento a ser pivoteado
  for(i=0; i< colunas;i++)
  {
    m[l][i]=m[l][i]/divisor;
    if(i < linhas -1)
    {
      certificado[l][i]=certificado[l][i]/divisor;
    }
  }

  //pivotear linhas de cima e de baixo
  //testar se o valor não é zero devido a divisão
  long double multiplicadorAuxiliar=0;
  for(i=0; i< linhas;i++)
  {
    //verifica praa pular a linha do pivô
    if (i != l)
    {
      //teste se o elemento já está zerado, se já estiver não é necessário operar sobre a linha
      if (m[i][c]!=0)
      {
        //fixando o valor que devemos multiplicar cada indice
        multiplicadorAuxiliar=m[i][c];
        for (j=0; j<colunas;j++)
        {
              //subtraindo a linha j para poder zerar o índice
              m[i][j]=m[i][j]- multiplicadorAuxiliar*m[l][j];
              if(j < linhas -1)
              {
                certificado[i][j]=certificado[i][j]- multiplicadorAuxiliar*certificado[l][j];
              }
        }
      }
    }
  }
}
int checarLinha(long double** m, int colunas,int linha)
{
  int validar =0;
  int i=0;
  //começa do 1 pois queremos checar as restriçoes
  for (i=0; i< colunas; i++)
  {
    if(m[linha][i]<0)
    {
      validar =1;
      i = colunas;
    }
  }
  return validar;
}
int checarColuna(long double** m, int linhas,int coluna)
{
  int validar =0;
  int i=0;
  //começa do 1 pois queremos checar as restriçoe
  for (i=1; i< linhas; i++)
  {
    if(m[i][coluna]>0)
    {
      validar =1;
      i = linhas;
    }
  }
  return validar;
}
int codicaoParadaDual(long double** m,int linhas, int colunas)
{
	int i=1;
	int validador=0;
	for(i=1;i<linhas;i++)
	{
		if(m[i][colunas-1]<0)
		{
			validador++;
			i=linhas;
		}
	}
	return validador;
}
int checarLinhaDual(long double** m, int linhas, int colunas,int posicao)
{
	int i =0;
	int validador=0;
	for(i=0;i<colunas-1;i++)
	{
		if(m[posicao][i] < -0.000001 && m[0][i]/-m[posicao][i] >=0.000001)
		{
			validador++;
			i = colunas;
		}
	}
	return validador;
}
int checarIlimitada(long double** m, int linhas, int colunas)
{
	int i=0;
	int j=0;
	int flag=0;
	for(i=0;i<colunas-1;i++)
	{
		if(m[0][i] < -0.000001)
		{

			for(j=0;j<linhas;j++)
			{
				if(m[j][i]>0.000001)
				{
					flag++;
				}
			}
			if(flag ==0)
			{
				return 1;
			}
		}
	}
	return 0;
}
int condicaoParadaDual(long double** m, int linhas, int colunas)
{
	int i =0;
	int auxiliar=0;
	for (i=1; i< linhas;i++)
	{
		if(m[i][colunas-1] <-0.000001)
		{
			auxiliar++;
			i=linhas;
		}
	}
	return auxiliar;
}
int checarInviabilidade(long double** m,int linhas, int colunas)
{
  int i=0;
  int j=0;
  int verificador=0;
  int verificador2=0;
  int inviabilidade=0;
  for(i=1; i< linhas; i++)
  {
    for(j=0;j<colunas-1;j++)
    {
      if(m[i][j]<0)
      {
        verificador++;
      }
      if(m[i][j]>0)
      {
        verificador2++;
      }
    }
    if((verificador == 0 && m[i][colunas-1] <0) || (verificador2 == 0 && m[i][colunas-1] >0))
    {
      inviabilidade =1;
    }
  }
  return inviabilidade;
}
Pivo defineElementoPivotearDual(long double** m, int linhas, int colunas)
{
 int i=0;
 Pivo p;
 p.linha = -1;
 p.coluna = -1;
 int flag_negatividade=0;
 long double menor;
 int contador =0;
 // verifica se já está na  solução
 if(condicaoParadaDual(m,linhas,colunas) == 0)
 {
	 p.coluna = -2;
	 p.linha = -2;
	 return p;
 }
 if(checarIlimitada(m,linhas,colunas) == 1)
 {
	 p.coluna = -3;
	 p.linha = -3;
	 return p;
 }
 //caso não esteja, comeca-se o processo de obter elemento a pivotear
 for (i=1; i< linhas; i++)
 {
	  //verifica se a entrada em b é negativa
    if(m[i][colunas-1]<-0.000001)
    {
      //verifica se existem entradas negativas na linha, caso tenha atribui esta linha a ser pivoteada
      if (checarLinhaDual(m,linhas,colunas,i) ==1)
      {
        p.linha =i;
        i = linhas;
			}
			contador ++;
    }
 }
 //se não existe linhas negativas, vamos checar os casos
 if (p.linha == -1)
 {
     p.coluna = -1;
		 printf("entrei aqi\n\n");
		 return p;
 }
 else
 {
   //pegando um valor qualquer de razão negativa para a menor razão
   for (i=0; i< colunas-1;i++)
   {
     if(m[p.linha][i]  < -0.000001)
     {
       if(m[0][i]/(-1*m[p.linha][i])>=-0.000001)
       {
         menor =  m[0][i]/(-1*m[p.linha][i]);
         i = colunas;
       }
     }
   }

   for (i=0; i< colunas-1; i++)
   {
     if(m[p.linha][i] < -0.000001)
     {
       if((m[0][i]/(-1*m[p.linha][i])) <= menor)
       {
         menor=m[0][i]/-m[p.linha][i];
         p.coluna=i;
       }
     }
   }
 }
 return p;
}
Pivo defineElementoPivotear(long double** m, int linhas, int colunas)
{
  // varre a primeira linha da matriz de forma encontrar o elemento negativo
 int i=0;
 Pivo p;
 p.linha = -1;
 p.coluna = -1;
 int flag_negatividade=0;
 long double menor=0;
 int contador =0;
 // definindo qual coluna a checar
 for (i=0; i< colunas; i++)
 {
    if(m[0][i]<0)
    {
			//coluna existe elemento >0, caso esteja -1 pl ilimitada
      if (checarColuna(m,linhas,i) ==1)
      {
        p.coluna =i;
        i = colunas;
      }
      contador ++;
    }

 }
 //definindo a linha
 if (p.coluna == -1)
 {
   if(contador ==0)
   {
     p.coluna = -2;
     p.linha = -2;
   }
   else
   {
		 p.linha = -1;
	 }

 }
 else
 {
   //pegando um valor qualquer de razão positivo para a menor razão
   for (i=1; i< linhas;i++)
   {
     if(m[i][colunas-1]/m[i][p.coluna]>=0 && m[i][p.coluna]>0)
     {
       menor =  m[i][colunas-1]/m[i][p.coluna];
       i = linhas;
     }
   }
   for (i=1; i< linhas; i++)
   {
     if(m[i][colunas-1]/m[i][p.coluna] <= menor && m[i][colunas-1]/m[i][p.coluna]>=0 && m[i][p.coluna] >0)
     {
       menor=m[i][colunas-1]/m[i][p.coluna];
       p.linha=i;
       if(m[i][colunas-1]/m[i][p.coluna]==0)
       {
         i = linhas;
       }
     }
   }
 }
 return p;
}
int checarVetorB(long double** m,int linhas, int colunas)
{
  int i=0;
  int valor=0;
  for(i=1;i<linhas;i++)
  {
    if(m[i][colunas-1] < -0.000001)
    {
      //vendo que pode ser solucionado pelo dual e saindo do loop
      valor =1;
      linhas = i;
    }
  }
  return valor;
}
int checarParadaPrimal(long double** m,int linhas, int colunas)
{
	int i;
	int validador=0;
	for(i=0;i<colunas-1;i++)
	{
		if(m[0][i] >=-0.000001)
		{

		}
		else
		{
			validador++;
		}
	}
	return validador;
}
int checarVetorC(long double** m,int linhas, int colunas)
{
  int i=0;
	int zero=0;
	int positividade=0;
	// i = 2, vetor todo negativo
	//i =0, vetor todo positivo
	//i=1, vetor com elementos positivo e negativo
  int valor=0;
	int negatividade=0;
	for(i=0;i<colunas;i++)
  {
		if(m[0][i] <= 0.000001 && m[0][i] >= -0.000001)
    {
      m[0][i] =0;
    }
		if(m[0][i] < 0)
    {
      negatividade++;
    }
		else if(m[0][i] > 0)
		{
			positividade++;
		}
		else
		{
			zero++;
		}
	}
	for(i=0;i<colunas;i++)
  {
    if(m[0][i] <= 0)
    {
      //vendo que pode ser solucionado pelo dual e saindo do loop
      valor =1;
      linhas = i;
    }
  }
	if(negatividade+zero == colunas)
	{

		valor =2;
	}
	else if(positividade+zero == colunas)
	{
		valor=0;
	}
	else
	{
		valor=1;
	}
  return valor;
}
int solverDual(long double** m,int linhas, int colunas,long double** certificado)
{
  int i;
  int j;
  Pivo p;
  int validador =1;
	int resultado=0;;
 while (validador)
  {
    p=defineElementoPivotearDual(m, linhas, colunas);
	  if(p.coluna == -1 )
    {
      printf("\n\nPl inviavel \n\n");
			printarMatriz(m,linhas,colunas);
			printf("\n\n\n");
			printf("%d",checarVetorC(m,linhas,colunas));
      printf("\n\n");
			resultado=0;
			validador =0;
    }
		else if (p.coluna == -3)
		{
			printf("\n\n pl Ilimitada \n\n");
			resultado=1;
			validador=0;

		}
    else if(p.linha == -2)
    {
			resultado=2;
      printf("\n\n");
      validador =0;

    }
    else
    {
			printarMatriz(m,linhas,colunas);
      printf("\n\n%d \t %d\n\n", p.linha, p.coluna);
      pivotear(m, p.linha, p.coluna,linhas,colunas,certificado);
			printf("\n\n");
    }
  }
	return resultado;
}

int solver(long double** m,int linhas, int colunas,long double** certificado)
{
  int i;
  int j;
  Pivo p;
	int resultado=0;
	// 0 para viavel, 1 inviavel, 2 ilimitada
	int validador =1;
  while (validador)
  {
    p=defineElementoPivotear(m, linhas, colunas);
    if(checarInviabilidade(m,linhas,colunas)==1 )
    {
      printf("\n\nPL inviável \n\n");
			resultado=0;
			validador =0;
    }
		else if( p.linha == -1)
		{
			printf("\n\n PL ilimitada\n\n");
			resultado=1;
			validador=0;
		}
    else if(checarParadaPrimal(m,linhas,colunas) == 0)
    {
			resultado=2;
      printf("\n\n");
      validador =0;
    }
    else
    {
      printf("\n\n%d \t %d\n\n", p.linha, p.coluna);
      pivotear(m, p.linha, p.coluna,linhas,colunas,certificado);
			printarMatriz(m,linhas,colunas);
      printf("\n\n");
    }
  }
	return resultado;
}
void colocaFPI(long double** m,int linhas, int colunas)
{
  int i=0;
  int j=0;
  //começamos do 1 pois a primeira linha é a função objetivo e iniciamos a matriz com 0
  for (i=1; i< linhas; i++)
  {
    m[i][colunas+i-1]=1;
  }
}
void extraiSolucao(long double** m,int linhas, int colunas,FILE *fp)
{
	int i=0;
	int j=0;
	int numUm=0;
	int lin;
	int contador=0;
	int numZero=0;
	for(j=0;j<colunas-linhas;j++)
	{
		for(i=0;i<linhas;i++)
		{
			if(m[i][j]==1)
			{
				numUm++;
				lin=i;
			}
			else if(m[i][j]==0)
			{
				numZero++;
			}
		}

		if(numUm==1 && numZero==linhas-1)
		{

			numUm=0;
			numZero=0;
			contador++;

			if(contador == linhas-2)
			{
					fprintf(fp,"%Lf\t",m[lin][colunas-1]);

			}
			else
			{
				fprintf(fp,"%Lf\t",m[lin][colunas-1]);

			}

		}
		else
		{
			if(contador == linhas-2)
			{
					fprintf(fp,"%d\t",0);
			}
			else
			{
				fprintf(fp,"%d\t",0);

			}
			numUm=0;
			numZero=0;

		}

	}


}
int checarColunaBasica(long double **k,int linhas,int coluna)
{
	int numZero=0;
	int numUm=0;
	int i=0;
	int coordenada;
	for(i=0;i<linhas;i++)
	{
		if(k[i][coluna] >= -0.000001 && k[i][coluna]<=0.000001)
		{
			numZero++;
		}
		else if(k[i][coluna] >= 1 && k[i][coluna]<=1.000001)
		{
			numUm++;
			coordenada=i;
		}
	}
	if(numZero + numUm == linhas)
	{
		return coordenada;
	}
	else
	{
		return -1;
	}
}
void extraiSolucao2(long double** m,long double* sol,int linhas, int colunas,int col)
{
	int i=0;
	int j=0;
	int coordenada=0;
	for(j=0;j<col;j++)
	{
		coordenada=checarColunaBasica(m,linhas,j);
		if(coordenada !=-1)
		{
			sol[j]=m[coordenada][colunas-1];
		}
		else
		{
			sol[j]=0;
		}
	}
}
int checaBase(long double ** auxiliar,int linhas, int coluna)
{
	int i=0;
	int j=0;
	int sol;
	int numUm=0;
	int lin;
	int contador=0;
	int numZero=0;

		for(i=0;i<linhas;i++)
		{
			//printf("valor %Lf\n",auxiliar[i][coluna]);
			if(auxiliar[i][coluna]==1 && i!=0)
			{
				numUm++;
				lin=i;
			}
			else if(auxiliar[i][coluna]==0)
			{
				numZero++;
			}
		}
		//printf("numUm %d numZero %d \n",numUm,numZero);
		if(numUm==1 && numZero==linhas-1)
		{
			//printf("Teste");
			numUm=0;
			numZero=0;
			contador++;
			sol = lin;

		}
		else
		{
			sol = -1;
		}
		return sol;
}
void extraiCertificadoIlimitada(long double ** auxiliar,int linhas, int colunas, FILE *fp)
{
	int j=0;
	int coordenada ;
	int sol;
	for(j=0;j<colunas-1;j++)
	{
		if(auxiliar[0][j] < 0)
		{
			coordenada=j;
			j=colunas;
		}
	}
	for(j=0;j<colunas-1;j++)
	{
		sol = checaBase(auxiliar,linhas,j);
		if(sol <0 && j != coordenada)
		{
			fprintf(fp,"0\t");
		}
		else if(j == coordenada)
		{
			fprintf(fp,"1\t");
		}
		else
		{
			fprintf(fp,"%Lf\t",-auxiliar[sol][coordenada]);
		}
	}
	printf("\n");
}
void extraiCertificado(long double** m,long double ** auxiliar,int linhas, int colunas,int solucao, int col)
{
	int i=0;
	int j=0;
	FILE *fp;
	if((fp = fopen("solucao.txt", "a+")) == NULL)
	{
		perror("Erro ao abrir solucao");
		exit(1);
	}
	if(solucao == 0)
	{
		fprintf(fp,"0 \n[");
		for(j=0;j<colunas;j++)
		{

				if(j == colunas-1)
				{
					fprintf(fp,"%Lf\t",m[0][j]);
				}
				else
				{
						fprintf(fp,"%Lf\t",m[0][j]);
				}
		}
		fprintf(fp,"]\n");
	}
	else if(solucao == 2)
	{
		fprintf(fp,"2 \n[");
		extraiSolucao(auxiliar,linhas,col,fp);
		fprintf(fp,"] \n  %Lf, \n[",auxiliar[0][col-1]);
		for(j=0;j<colunas;j++)
		{
				if(j == colunas-1)
				{
					fprintf(fp,"%Lf",m[0][j]);
				}
				else
				{
						fprintf(fp,"%Lf,",m[0][j]);
				}
		}
		fprintf (fp,"]\n");
	}
	else
	{
		fprintf(fp,"1 \n[");
		extraiCertificadoIlimitada(auxiliar,linhas,col,fp);
		fprintf(fp,"]");
	}
	fclose(fp);

}
void montaTableaux(long double** m,int linhas, int colunas,long double** certificado)
{
  int i=0;
  int j =0;
  //multiplicar a primeira linha por -1
  for(i=0; i< colunas;i++)
  {
    m[0][i] = m[0][i]*(-1);
  }
  for (i=1; i< linhas; i++)
  {
    certificado[i][i-1]=1;
  }

}
void simplexDual(long double** m,int linhas, int colunas)
{
  int i=0;
	int solucao=0;
	long double ** certificado = alocarMatriz(linhas,linhas-1);
  montaTableaux(m,linhas,colunas,certificado);
  solucao = solverDual(m,linhas,colunas,certificado);
	if(solucao == 2)
	{
		//solução
		extraiCertificado(certificado,m,linhas,linhas-1,solucao,colunas);
	}
	else if(solucao ==1)
	{
		extraiCertificado(certificado,m,linhas,linhas-1,solucao,colunas);
	}
	else
	{
		extraiCertificado(certificado,m,linhas,linhas-1,solucao,colunas);
	}
  desalocarMatriz(linhas,certificado);
  free(certificado);
}


void copiarMatriz(long double** m,long double** k,int linhas, int colunas)
{
	int i=0;
	int j=0;
	for(i=0;i<linhas;i++)
	{
		for(j=0;j<colunas;j++)
		{
			m[i][j]=k[i][j];
		}
	}
}
void simplexPrimal(long double** m,int linhas, int colunas)
{
  int i=0;
	int solucao=0;
	char *mensagem;
	int flag;
	int basica =0;

	long double ** certificado = alocarMatriz(linhas,linhas-1);
  montaTableaux(m,linhas,colunas,certificado);
  solucao =solver(m,linhas,colunas,certificado);
	extraiCertificado(certificado,m,linhas,linhas-1,solucao,colunas);
  desalocarMatriz(linhas,certificado);
	free(certificado);
}


void montarCAuxiliar(long double** auxiliar,int linhas, int colunas)
{
	int i=0;
	int j=0;
	for (i=0;i<colunas+linhas-1;i++)
	{
		if(i< colunas-1)
		{
			auxiliar[0][i]=0;
		}
		else
		{
			if(i==colunas+linhas-2)
			{
					auxiliar[0][i]=0;
			}
			else
			{
				auxiliar[0][i]=-1;
			}

		}
	}
}
void montarAAuxiliar(long double** auxiliar,long double** certificado,long double** m,int linhas, int colunas, int contador)
{
	int i=0;
	int j=0;
	for(i=1;i<linhas;i++)
	{
		for(j=0; j<colunas-1;j++)
		{
			if(m[i][colunas-1]< 0)
			{
				auxiliar[i][j] = -m[i][j];
				auxiliar[0][j]+=m[i][j];
				certificado[0][i]+=-1;
				certificado[i][i]*=-1;
			}
			else
			{
				auxiliar[i][j] = m[i][j];
			}

		}
	}
}
void montarBAuxiliar(long double** auxiliar,long double** m,int linhas, int colunas, int contador)
{
	int i=0;
	int j=0;
	int aux=1;
	for(i=1;i<linhas;i++)
	{
		  if(m[i][colunas-1] < 0)
			{
				auxiliar[i][colunas-1+contador]=-m[i][colunas-1];
				auxiliar[i][colunas-2+aux]=1;
				auxiliar[0][colunas+contador-1]+=-auxiliar[i][colunas+contador-1];
				aux++;
			}
			else
			{
				auxiliar[i][colunas-1+contador]=m[i][colunas-1];
			}

		}
}
void extraiB(long double** m,long double** auxiliar,int linhas, int colunas)
{
	int i=0;
	int j=0;
	int numUm=0;
	int lin=0;
	int contador=0;
	int numZero=0;
	for(j=0;j<colunas-linhas;j++)
	{
		for(i=0;i<linhas;i++)
		{
			if(m[i][j]==1)
			{
				numUm++;
				lin=i;
			}
			else if(m[i][j]==0 || (m[i][j] < 0.000001 && m[i][j] > -0.000001 ))
			{
				numZero++;
			}
		}

		if(numUm==1 && numZero==linhas-1)
		{
			numUm=0;
			numZero=0;
			m[lin][colunas-1]=auxiliar[lin][colunas-1];
		}
		else
		{
			m[lin][colunas-1]=0;
		}

	}
}
int quantidadeLinhasNegativas(long double** m,int linhas, int colunas)
{
	int i =0;
	int contador=0;
	for(i=1; i < linhas;i++)
	{
		if(m[i][colunas-1]<0)
		{
			contador++;
		}
	}
	return contador;
}
void auxiliar(long double** m,int linhas, int colunas)
{
  int i=0;
	int j=0;
	int solucao;
	int contador =quantidadeLinhasNegativas(m,linhas,colunas);
	printf("\n\n %d \n\n",contador);
	long double ** auxiliar = alocarMatriz(linhas,colunas + contador);
	long double ** certificado = alocarMatriz(linhas,linhas-1);
	for (i=1; i< linhas; i++)
	{
	   certificado[i][i-1]=1;
	}
	montarAAuxiliar(auxiliar,certificado,m,linhas,colunas,contador);
	montarBAuxiliar(auxiliar,m,linhas,colunas,contador);
	solucao=solver(auxiliar,linhas,colunas+contador,certificado);

 	if(auxiliar[0][colunas+contador-1] <= 0.000001 && auxiliar[0][colunas+contador-1] >= -0.000001 )
 	{
	 	auxiliar[0][colunas+contador-1]=0;
 	}
	char *mensagem;
	if(solucao ==2 && auxiliar[0][colunas+contador-1] != 0)
	{
		solucao =0;
		extraiCertificado(certificado,auxiliar,linhas,linhas-1,solucao,colunas+contador);
		desalocarMatriz(linhas,certificado);
		free(certificado);
	}
	else if(solucao ==2 && auxiliar[0][colunas+contador-1] == 0)
	{
		desalocarMatriz(linhas,certificado);
		free(certificado);
		extraiB(m,auxiliar,linhas,colunas);
		simplexPrimal(m,linhas,colunas);
		//setando vetor b na pl original
	}
	else
	{
		extraiCertificado(certificado,auxiliar,linhas,linhas-1,solucao,colunas);
	}
  desalocarMatriz(linhas,auxiliar);
  free(auxiliar);

}
int decideSimplex(long double** m,int linhas, int colunas)
{
  int valorB = checarVetorB(m,linhas,colunas);
	int valorC =checarVetorC(m,linhas,colunas);
	// c = 2, todo negativo, c=0, todo positivo, c=1 negativo e positivo
	// b=1 existe negativo, b=0 todo positivo
	printf("\n%d %d\n", valorB,valorC);
	if( valorB == 1 && valorC==1)
  {
		//Pl auxiliar
		printf("tentei auxiliar\n");
		auxiliar(m, linhas,  colunas);
    //
  }
	else if(valorB == 0 && valorC==0 ||valorB == 0 && valorC==1 )
	{
		printf("tentei primal\n");
		simplexPrimal(m,linhas,colunas);
	}
	else if(valorB ==1 && valorC==2)
	{
		printf("tentei dual\n");
		simplexDual(m,linhas,colunas);
	}
}
//função para receber a pl
long double** recebePL(int* linhas, int* colunas, int *op)
{
  int i=0;
  int j=0;
	char c;
	char nome[100];
	printf("Digite o nome do arquivo:\n");
	scanf("%s",nome);
	FILE *fp;
	 fp = fopen (nome, "rw");
	 if (fp == NULL) {
			printf ("Houve um erro ao abrir o arquivo.\n");

	 }
	 int cont=0;
	 fscanf(fp,"%d \n %d\n",linhas,colunas);
	 //aloca espaço para poder colocar em fpi imediatamente
	 long double t;
   long double ** m = alocarMatriz(*linhas+1,*colunas+*linhas+1);
	 while((c = getc(fp) ) != EOF)
	 {
		 	if(c =='[' || c ==',')
			{
				cont++;
			}
			if(cont ==2)
			{
				for(i=0;i<*colunas+1;i++)
				{
					if(i == *colunas)
					{
							fscanf(fp,"%Lf",&m[j][*colunas+*linhas]);
					}
					else
					{
						fscanf(fp,"%Lf,",&m[j][i]);
					}

				}
				j++;
				cont=0;
			}
	 }
	 fclose (fp);
	 return m;
}


int main ()
{
  int linhas =0 ;
  int colunas =0;
  int i=0;
  int j=0;
	int auxiliar=1;
	int tam=0;
	int op;
	long double** m=recebePL(&linhas,&colunas,&op);
	long double otimo = -10^9;
	//coloca a entrada em fpi
	colocaFPI(m,linhas+1,colunas);
	decideSimplex(m,linhas+1,colunas+linhas+1);
	tam = colunas;
	desalocarMatriz(linhas+1,m);
    free(m);
	return 0;
}
