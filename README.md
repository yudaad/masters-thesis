# masters-thesis
This is my masters thesis. I examine the relationship between migration experience to big cities on income, both formal and informal workers based on Indonesia Family Life Survey (IFLS) dataset. I am inspired by Roca &amp; Puga (Restud, 2017). IFLS is a household survey conducted by RAND Corporation, United States. Currently, IFLS has been done in 5 waves of survey. IFLS 2014 (wave 5) is currently the latest survey.   

In this research, I explore the role of migration experience to Jabodetabek and other metropolitan areas on workers' income. The hypothesis here is that big cities offer many skilled workers, so that they will interact and sharing knowledge with each other in that it will accelerate human capital accumulation and learning process. Once workers leaving those big cities, they will become the highest income earner in smaller cities because of skill they have already gained in big cities. The result of this research, contradicting with my hypothesis, suggest that workers with experience in Jabodetabek doesn't have higher income. This is because workers who have experience migrating to Jabodetabek mainly come from long-distance city, such that migration cost is higher. Also, Jabodetabek, with 10+ millions citizens, creates what economists called by '*demons of density*', inequality, informality, congestion, poverty, urban polarization, and high cost of living. These *demons*, in turn, create concept '*survival of the fittest*', those who are able to deal with such density cost, will survive and be benefited from agglomeration economies, both formal and informal workers. Meanwhile, unlike workers with Jabodetabek experience, workers with migration experience to metropolitan areas receive higher income. This is because they come from near-distance areas/cities from metropolitas areas. Hence, migration cost is lower. For informal workers, near-distance means they can build network externalities to develop their businesses.  

Next, I will explain what variables are used in this research:  

* Dependen Variable: `Log Income`  
Obtained from book `b3a_tk2`, and calculated by summing up 4 components:  
  + `tk25a1` : Wage from main job
  + `tl26a1` : Profit from main job (self-employed)
  + `tk25b1` : Wage from side job
  + `tk26b1` : Profit from side job

* Main Variable Independent:
  + Jabodetabek Migration Experience (Dummy variable, taken from book `b3a_mg2`)
    - `1` : Has experience migrating to Jabodetabek for non-Jabodetabek and non-metropolitan worker
    - `0` : Doesn't have experience migrating to Jabodetabek for non-Jabodetabek and non-metropolitan worker
  + Metropolitan Migration Experience (Dummy taken from book `b3a_mg2`)
    - `1` : Has experience migrating to metropolitan for non-Jabodetabek and non-metropolitan worker
    - `0` : Doesn't have experience migrating to metropolitan for non-Jabodetabek and non-metropolitan worker
  + Jabodetabek (Currently living when the survey conducted, taken from book `bk_sc1`)
    - `1` : Currently live in Jabodetabek (in the time survey conducted)
    - `0` : Currently doesn't live in Jabodetabek (in the time survey conducted)
  + Metropolitan (Currently living when the survey conducted, taken from book `bk_sc1`)  
    - `1` : Currently live in metropolitan (in the time survey conducted)
    - `0` : Currently doesn't live in metropolitan (in the time survey conducted)

* Variabel Independent (Control):
  + Formal/Informal Worker (Dummy variable, taken from book `b3a_tk2` variable `tk24a`)
    - `1` : Formal worker (private worker, government worker, and self-employed with permanent worker)
    - `0` : Informal worker (self-employed with family worker, self-employed with temporary worker, unpaid family worker, casual worker in agriculture, casual worker in non-agriculture)
  + Work sector (Factor variable, 9 sectors. Taken from book `b3a_tk2` variable `tk19ab`)
  + Tenure (Integer, How many years one has been working in the current job. Taken from book `b3a_tk2` variable `tk23a2y`)
  + Years of Schooling (Integer, years spent on school. Taken from book `b3a_dl2`)
  + Household Head (Dummy variable, is one a household head. Taken from book `bk_ar1`)
    - `1` : Household head
    - `0` : Not a household head
  + Household Size (Integer, how many family member in the household. Taken from book `bk_ar1` variable `ar02b`)
  + Gender (Dummy variable, taken from book `bk_ar1` variable `ar07`)
    - `1` : Male
    - `0` : Female
  + Age (Integer, taken from book `bk_ar1` variable `ar09`)
  + Marital status (Dummy variable, taken from book `bk_ar1` variable `ar13`)
  + Gross Domestic Regional Bruto (Numeric, taken from Simreg Bappenas BPS)
  + Minimum Wage (Numeric, taken from BPS)




