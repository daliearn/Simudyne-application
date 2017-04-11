#Arnaud DALIE Back-End Test Simudyne

##########################################################
#REQUIREMENT										     #
#INSTALL R 3.3.3 x64 to run the script                   #
#(however, I recommand to read the code on a text Editor)#
#BE SURE TO GET JAVA 64BIT ON YOUR COMPUTER			     #	
#IN THE R SHELL USE THE FOLLOWING COMMANDS :             #
#install.packages("XLConnect")						     #
#install.packages("rJava")							     #
#install.packages("RecordLinkage")					     #
#setwd("[PATH to Work Directory]")					     #
##########################################################

setwd("C:/Users/Utilisateur/Desktop/Simudyne")	
#Loading Packages
require(rJava)
require (XLConnect)
require (RecordLinkage)
		
#Loading Excel sheet
#I could have converted it to CSV but I choose not to do it
table = loadWorkbook("Simudyne_Backend_Test.xlsx")
tab = readWorksheet(table, sheet = "Data", header = TRUE)

l = length(tab$Age)
strBreed_C = "Breed_C"
strBreed_NC = "Breed_NC"

#Set Up of output variables
Affinity  = c(1:l)
temp_Breed_C = c(1:l)
temp_Breed_NC = c(1:l)
temp_Breed_C_Lost = c(1:l)
temp_Breed_C_Gained = c(1:l)
temp_Breed_C_Regained = c(1:l)
Breed_C_Lost = c(0)
Breed_C_Gained = c(0)
Breed_C_Regained = c(0)

#The idea is to make temp_Vectors (like temp_Breed_C) for each year and then concatenate them into a large vector (like Breed_c_Gained)
#These vectors are made of 0 and 1 that mean False/True : if temp_Breed_C[i] = 1 then the element i is a Breed_C 

#Initialization of Breed_C and Breed_NC output vectors
#for year 0 (Initial values)
for (i in 1:l){
	if (jarowlinker(tab$Agent_Breed,strBreed_C)){
		#jarowlinker is a strcmp()
		temp_Breed_C = c(temp_Breed_C,i)
	}
	if (jarowlinker(tab$Agent_Breed,strBreed_NC)){
		temp_Breed_NC = c(temp_Breed_NC,i)
	}
}
Breed_C= temp_Breed_C
Breed_NC = temp_Breed_NC


#Beginning of the implementation
for (year in 1:15){
	
	tab$Age <- tab$Age +1

	#I choose to go in vector element by element
	for(i in 1:l){	

		#Each element of temp_vectors are set to 0 except temp_Breed_NC and temp_Breed_Lost  (used to compute temp_Breed_C_regained)
		temp_Breed_C[i] = 0
		temp_Breed_C_Gained[i] = 0
		temp_Breed_C_Regained[i] = 0
		
		if (tab$Auto_Renew[i]  == 1){ 
		}else{
			rand <- runif(1,0,3) #genrate a random number in [0;3]
			
			#I compute a Vector Affinity to make if statement simpler
			Affinity[i] <- (tab$Payment_at_Purchase[i]/tab$Attribute_Price[i]) + (rand * tab$Attribute_Promotions[i] * tab$Inertia_for_Switch[i])
			
			#I simulated a model input with some random values in [0.1;2.9]
			model_input = runif(1,0.1,2.9)

			#I strugled a lot with boolean and if statements in R 
			#So I decided to use "fake booleans" that are integer

			BreedBool = 0
			if(jarowinkler(tab$Agent_Breed[i],strBreed_NC) ){BreedBool = 1}

			AffinityBool = 0
			tempvar = tab$Social_Grade[i] * tab$Attribute_Brand[i] * model_input
			if(tempvar >= Affinity[i]){AffinityBool = 1}

			
			#This condition is like if(Breebool && AffinityBool){}
			if (BreedBool + AffinityBool == 2)
				{

					tab$Agent_Breed[i]<-strBreed_C        #We change the value in the data.frame
					temp_Breed_C[i] = 1            		  #We get a new Breed_C so we put the value to 1
					if (temp_Breed_NC[i] == 1){	   		  #If it was in NC, it's a gained Breed_C
						temp_Breed_C_Gained[i] = 1
						temp_Breed_NC[i] = 0			  #We didn't reset temp_Breed_NC and obviously we cant have Breed_NC and Brees_Gained at 1
														  #for the same element

						if(temp_Breed_C_Lost[i] == 1){		  #If it's gained and it was previously Lost
							temp_Breed_C_Regained[i] = 1  #Then it's regained
						}
					}
				}

			else{

				#Same idea than above

				BreedBool = 0
				
				if(jarowinkler(tab$Agent_Breed[i],strBreed_C) ){BreedBool = 1}
				
				
				AffinityBool = 0
				if((tab$Social_Grade[i] * tab$Attribute_Brand[i]) >= Affinity[i])
					{AffinityBool = 1}
							

				if (BreedBool + AffinityBool == 2)
					{
						if (jarowinkler(tab$Agent_Breed[i],strBreed_C)){ #If it was a Breed_C, it will became a NC and it's lost
							temp_Breed_C_Lost[i] = 1
						}else{temp_Breed_C_Lost[i] = 0}                  #Remember that we din=dn't reset temp_Breed_C_Lost
																		 #We do it now
						tab$Agent_Breed[i]<-strBreed_NC
						temp_Breed_NC[i] = 1
					}


			}
		}
	}

	#Finnally we concatenate the temp_vectors into the big ones
	Breed_C = c(Breed_C,temp_Breed_C)
	Breed_NC = c(Breed_NC,temp_Breed_NC)
	Breed_C_Gained = c(Breed_C_Gained,temp_Breed_C_Gained)
	Breed_C_Regained = c(Breed_C_Regained,temp_Breed_C_Regained)
	Breed_C_Lost = c(Breed_C_Lost,temp_Breed_C_Lost)

}

#I will now save tab in an excel sheet
fileXls <- paste("C:/Users/Utilisateur/Desktop/Simudyne","result.xlsx",sep='/')
unlink(fileXls, recursive = FALSE, force = FALSE)
exc <- loadWorkbook(fileXls, create = TRUE)

createSheet(exc,'FinalTab')
writeWorksheet(exc, tab, sheet = "FinalTab", startRow = 1, startCol = 1)

saveWorkbook(exc)

#NOTES :
#there are some things to do to make this code better
#1)try to compact it : since we know it's working we can compact the code in if () statements 
#  it means to make bigger conditions to get less lines of codes
#2)Vectorize the code : R isn't my favorite language so i didn't manage to implement this script
#  without usin a for loop from 1 to length
#  Using a Vectorized implementation, the code would run much Faster.
#3)I should find a way to present my output better using this sample of Code (not working currently)
#createSheet(exc,'Breed_C')
#createSheet(exc,'Breed_C_Regained')
#createSheet(exc,'Breed_NC')
#createSheet(exc,'Breed_C_Lost')
#createSheet(exc,'Breed_C_Gained')

#writeWorksheet(exc, Breed_), sheet = "Breed_C", startRow = 1, startCol = 1)
#writeWorksheet(exc, Breed_NC, sheet = "Breed_NC", startRow = 1, startCol = 1)
#writeWorksheet(exc, Breed_C_Lost, sheet = "Breed_C_Lost", startRow = 1, startCol = 1)
#writeWorksheet(exc, Breed_C_Gained, sheet = "Breed_C_Gained", startRow = 1, startCol = 1)
#writeWorksheet(exc, Breed_C_Regained, sheet = "Breed_C_Regained", startRow = 1, startCol = 1)