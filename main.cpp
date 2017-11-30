#include <fstream>
#include <iostream>
#include "cctype"
#include <math.h>
using namespace std;

void del_mem(float**,unsigned int,unsigned int );

float ** create_mat ( unsigned int stroki,unsigned int stolbi) {
    float ** mat=new float*[stroki];
    for (int i=0; i<stroki; i++) {
        mat[i]=new float [stolbi];
    }
    return mat;
}

int proverka(unsigned int stroki,unsigned int stolbi,unsigned int stroki2,unsigned int stolbi2){
    if(stolbi==stolbi2 && stroki==stroki2) return 1;
    else if(stroki==stolbi2) return 0;
    return -1;
}

float ** plus_mat(float **mat1,float **mat2,unsigned int stroki,unsigned int stolbi,unsigned int stroki2,unsigned int stolbi2){
    if(proverka( stroki,stolbi, stroki2, stolbi2)!=1){
        cout<<"Error add"<<endl;
        del_mem(mat1, stroki, stolbi);
        del_mem(mat2, stroki2, stolbi2);
        exit(1);
    }
    float **mat3 = create_mat( stroki,stolbi);
    for(int i=0;i<stroki;i++){
        for(int j=0;j<stolbi;j++){
            mat3[i][j]=mat1[i][j]+mat2[i][j];
        }
    }
    return mat3;
}


float ** minus_mat(float **mat1,float **mat2,unsigned int stroki,unsigned int stolbi,unsigned int stroki2,unsigned int stolbi2){
    if(proverka( stroki,stolbi, stroki2, stolbi2)!=1){
        cout<<"Error sub"<<endl;
        del_mem(mat1, stroki, stolbi);
        del_mem(mat2, stroki2, stolbi2);
        exit(1);
    }
    
    float **mat3 = create_mat( stroki,stolbi);
    for(int i=0;i<stroki;i++){
        for(int j=0;j<stolbi;j++){
            mat3[i][j]=mat1[i][j]-mat2[i][j];
        }
    }
    return mat3;
}

float ** umnog_mat(float **mat1,float **mat2,unsigned int stroki,unsigned int stolbi,unsigned int stroki2,unsigned int stolbi2){
    if(proverka(  stroki,stolbi, stroki2, stolbi2)==-1){
        cout<<"Error mult"<<endl;
        del_mem(mat1, stroki, stolbi);
        del_mem(mat2, stroki2, stolbi2);
        exit(2);
    }
    float **mat3=create_mat(stroki,stolbi2);
    for(int i = 0; i < stroki; i++){
        for(int j = 0; j < stolbi2; j++)
        {
            mat3[i][j] = 0;
            for(int k = 0; k < stroki2; k++)
                mat3[i][j] += mat1[i][k] * mat2[k][j];
        }
    }
    return mat3;
}


float ** transp_mat(int stroki,int stolbi,float **mat1){
    float ** mat3= create_mat(stolbi,stroki);
    
    for(int i = 0; i<stolbi; i++){
        for(int j = 0; j<stroki; j++){
            mat3[i][j] = mat1[j][i];
        }
    }
    return mat3;
}

bool issqure(unsigned int stroki,unsigned int stolbi){
    if (stroki==stolbi) return true;
    else return false;
}

float opredel ( float ** ,unsigned int);

void Short_Mat(float **,float**,int ,int,unsigned int);

float **obrat(float **mat1,unsigned int stroki,unsigned int stolbi){
    if( issqure(stroki,stolbi) ){
        if(opredel(mat1,stroki)!=0){
            float **M;
            M=new float*[stroki];
            for (int i=0; i<stroki; i++) {
                M[i]=new float[stroki];
            }
            float **V;
            V=new float*[stroki];
            for (int i=0; i<stroki; i++) {
                V[i]=new float[stroki];
            }
            int x=0;
            for(int i=0;i<stroki;i++){
                for (int j=0; j<stroki; j++) {
                    Short_Mat(mat1,V,i,j,stroki);
                    float m =opredel(V,stroki-1);
                    x=i+j;
                    if((x%2)!=0) m=m*(-1);
                    M[i][j]=m;
                }
            }
            M=transp_mat(stroki,stolbi,M);
            float a =opredel(mat1,stroki);
            for(int i=0;i<stroki;i++){
                for (int j=0; j<stroki; j++) {
                    M[i][j] *= (1/a);
                }
                
            }
            
            return M;
        }
        
    }
    cout<<"There is no reverse matrix"<<endl;
    del_mem(mat1, stroki, stolbi);
    exit(3);
}


void Short_Mat(float **mat1,float **p, int i, int j,unsigned int stroki ){
    int ki, kj, di, dj;
    di = 0;
    for (ki = 0; ki<stroki - 1; ki++) {
        if (ki == i) di = 1;
        dj = 0;
        for (kj = 0; kj<stroki - 1; kj++) {
            if (kj == j) dj = 1;
            p[ki][kj] = mat1[ki + di][kj + dj];
        }
    }
}

float opredel(float **mat1,unsigned int stroki){
    int i,d,k,n,j;
    
    float **p;
    p=new float*[stroki];
    for(int i=0;i<stroki;i++){
        p[i]=new float[stroki];
    }
    j=0;
    d=0;
    k=1;
    n=stroki-1;
    if (stroki<1) {cout << "An error has occured while reading input data"; exit(0);}
    if (stroki == 1) {
        d = mat1[0][0];
        return(d);
    }
    if (stroki == 2) {
        d = mat1[0][0] * mat1[1][1] - (mat1[1][0] * mat1[0][1]);
        return(d);
    }
    if (stroki>2) {
        for (i = 0; i<stroki; i++) {
            Short_Mat(mat1, p, i, 0, stroki);
            d = d + k * mat1[i][0] * opredel(p, n);
            k = -k;
        }
    }
    for (int i=0; i<stroki; i++) {
        delete [] p[i];
    }
    delete [] p;
    
    return(d);
}

void del_mem(float **mat,unsigned int stroki,unsigned int stolbi){
    for (int i=0; i<stroki; i++) {
        delete [] mat[i];
    }
    delete [] mat;
}

void out(float **mat3,unsigned int stroki,unsigned int stolbi){
    
    for (int i=0; i<stroki; i++) {
        for (int j=0; j<stolbi; j++) {
            if (mat3[i][j]==-0) {
                cout<<"0"<<" ";
            }
            else{
            cout<<mat3[i][j]<<" ";
            }
        }
        cout<<endl;
    }
    del_mem(mat3, stroki, stolbi);
}

float get_float(FILE*f){
    float chislo=0;
    char symb;
    bool flag=false;
    
    do {
        symb=fgetc(f);
}
    while(!((48<=symb && symb<=57) || symb=='-'));
    
    if (symb=='-') {
        flag=true;
        symb=fgetc(f);
    }
    
    do {
        chislo=chislo*10+symb-'0';
        symb=fgetc(f);
    }
    while(48<=symb && symb<=57);
    
    if (symb=='.' && isnumber(symb=fgetc(f))) {
        float drob_part=0;
        int razriad=0;
        do {
            razriad=razriad-1;
            drob_part=drob_part+(symb-'0')*pow(10,razriad);
            symb=fgetc(f);
        }
        while(48<=symb && symb<=57);
        chislo=chislo+drob_part;
    }
    
    return flag==false ? chislo:chislo*-1;
}

int get_int(FILE*);

float **mat(FILE * f,int &stroki,int &stolbi){
    stroki=get_int(f);
    stolbi=get_int(f);
    float **p_mat=create_mat(stroki, stolbi);
    for (int i=0; i<stroki; i++) {
        for (int j=0; j<stolbi; j++) {
            p_mat[i][j]=get_float(f);
        }
    }
    return p_mat;
}

int get_int(FILE*f){
     return int(get_float(f));
}

void scan_command(char c1[256],char c2[256],char &op){
    int i;
    for (i = 0; !isspace(c1[i]=getchar()); ++i)
    {
    };
    c1[i] = '\0';
    bool flag=true;
    bool bin_op=false;
    do {
        cin>>op;
        switch (op) {
            case '+':
                
            case '-':
                
            case '*':
                getchar();
                for (i = 0; !isspace(c2[i]=getchar()); ++i)
                {
                };
                c2[i] = '\0';
                bin_op=true;
            case 'T':
                
            case 'R':
                flag=false;
                break;
            default:
                cout<<"Error"<<endl;
                break;
        }
        
    }
    while(flag);
}

int main()
{
    char c1[256]="";
    char c2[256]="";
    int stroki1,stolbi1;
    float **p_mat1=nullptr;
    int stroki2 = 0;
    int stolbi2 = 0;
    float **p_mat2=nullptr;
    char op;
    scan_command(c1,c2,op);
    FILE *f1= fopen(c1,"r");
    p_mat1=mat(f1,stroki1,stolbi1);
    fclose(f1);
    
    switch (op) {
        case 'T':
            out(transp_mat(stroki1,stolbi1,p_mat1),stolbi1,stroki1);
            break;
        case 'R':
            out(obrat(p_mat1,stroki1,stolbi1),stroki1,stolbi1);
            
            break;
        case '+':
            if (c2[0]!='\0')
            {
                FILE *f2= fopen(c2,"r");
                p_mat2=mat(f2,stroki2,stolbi2);
                fclose(f2);
            out(plus_mat(p_mat1,p_mat2,stroki1,stolbi1,stroki2,stolbi2),stroki1,stolbi1);
            }
            break;
        case '-':
            if (c2[0]!='\0')
            {
                FILE *f2= fopen(c2,"r");
                p_mat2=mat(f2,stroki2,stolbi2);
                fclose(f2);
            
            out(minus_mat(p_mat1,p_mat2,stroki1,stolbi1,stroki2,stolbi2),stroki1,stolbi1);
            }
            break;
        case '*':
            if (c2[0]!='\0')
            {
                FILE *f2= fopen(c2,"r");
                p_mat2=mat(f2,stroki2,stolbi2);
                fclose(f2);
            
            out(umnog_mat(p_mat1,p_mat2,stroki1,stolbi1,stroki2,stolbi2),stroki1,stolbi2);
            }
            break;
        default:
            cout<<"An error has occured while reading input data"<<endl;
            break;
    }
    del_mem(p_mat1, stroki1, stolbi1);
    if (p_mat2!=nullptr) del_mem(p_mat2, stroki2, stolbi2);
    return 0 ;
}

