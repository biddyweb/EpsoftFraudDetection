f = open('d:\R_WORKSPACE\EPsoft\data\√≈’Ô∏ﬂ—™—π.txt')
output = open('result1.txt','w')
for i in f:
 #   print i
    flag = 0
    c = ''
    for j in i:
        if((j==',')&(flag==1)):
  #          print 'HAHA'
            c = c+';'
        else:
            c = c+j
        if(j=='{'):
            flag = 1
        if(j=='}'):
            flag = 0
   # print c
        
    output.writelines(c)
output.close()

