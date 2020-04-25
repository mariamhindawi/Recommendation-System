import System.Random 
import System.IO.Unsafe
randomZeroToX :: Int -> Int

randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))


helperreturnindex list =  returnindex (randomZeroToX ((length list)-1)) list


returnindex  x []=[]
returnindex  x (h:t) = if (x == 0) then h else if (  x > 0 ) then returnindex (x-1) t else []

users = [" user1", "user2", "user3", "user4 "] 
items = ["item1", "item2", "item3", "item4", "item5", "item6"] 
purchasesHistory = [ ("user1", [["item1", "item2", "item3"] , ["item1", "item2", "item4"]]) ,
				  ("user2", [["item2", "item5"] , ["item4", "item5"]]) , 
				  ("user3", [["item3", "item2"]]) , 
				  ("user4", []) ]


createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (h:t) = (h , [] ) : createEmptyFreqList t

getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats []=[]
getAllUsersStats ((user,carts):t) = (user , getUserStats (createEmptyFreqList items) carts) : getAllUsersStats t

getUserStats acc [] = acc
getUserStats acc (cart:carts) = getUserStats (addFreq acc (count items items cart)) carts

addFreq acc [] = acc
addFreq acc (x:xs) = addFreq (addFreqFinder x acc) xs

addFreqFinder _ [] = []
addFreqFinder (item, freqList) ((i, list):xs) = if i == item then (i, addPairForAllItems freqList list):xs
															else (i,list):addFreqFinder (item, freqList) xs

addPairForAllItems [] list = list
addPairForAllItems (x:xs) list = addPairForAllItems xs (addPair x list)

addPair a [] = [a]
addPair (item, count) ((i, c):xs) = if item == i then (i, c + count):xs
												else (i,c): addPair (item, count) xs

 
checkCount :: String -> [String] -> Int
checkCount _ [] = 0
checkCount str (x:xs) | str == x = 1 + checkCount str xs
				   | otherwise = 0 + checkCount str xs
 
count [] _ _ = []
count (x:xs) totalItems cart = if elem x cart then (x, countNotMe x totalItems cart): count xs totalItems cart
												else count xs totalItems cart

countNotMe _ [] _ = []		 
countNotMe item (x:xs) cart = if item /= x then if count /= 0 then  (x, checkCount x cart): countNotMe item xs cart 
															else countNotMe item xs cart
										else countNotMe item xs cart
										where count = checkCount x cart
				   

 

freqListItems:: String -> [(String, Int)]
freqListItems user = counting user (getAllUsersStats purchasesHistory)
counting user ((u,(item,items):t1):t)  = if ( user == u ) then helpercounting items t1 else counting user t
helpercounting items [] = items
helpercounting items ((i,list):x)=   helpercounting (addPairForAllItems items list) x



freqListCart:: String ->[String] -> [(String, Int)] 
freqListCart user hiscart = helperfreqlistcart user hiscart (getAllUsersStats purchasesHistory)
helperfreqlistcart _ _ []=[]
helperfreqlistcart user hiscart ((u,hiscarts):t) = if (user == u) then helperfreqlistcart2 hiscart hiscarts else  helperfreqlistcart user hiscart t
helperfreqlistcart2 [] _=[]
helperfreqlistcart2 _ [] =[]
helperfreqlistcart2 (h:t) ((item,items):t1) = if (h==item) then addPairForAllItems (items) (helperfreqlistcart2 t t1)  else helperfreqlistcart2 (h:t) t1 

													 
freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user hiscart= helperoff (freqListCart user hiscart) (freqListItems user)
helperoff list list1 =addPairForAllItems list list1 

recommendEmptyCart :: String -> String
recommendEmptyCart user = helperreturnindex (helperrecommendEmptyCart2 (freqListItems user))

helperrecommendEmptyCart (_,0)=[]
helperrecommendEmptyCart (l,x)= (l:helperrecommendEmptyCart (l,x-1))

helperrecommendEmptyCart2 []=[]
helperrecommendEmptyCart2 ((x,y):n) = helperrecommendEmptyCart (x,y) ++helperrecommendEmptyCart2 n

 

recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user hiscart = helperreturnindex(helperrecommendEmptyCart2 (freqListCartAndItems user hiscart))

purchasesIntersection :: Eq a => [(a,[(a, Int)])] -> [(a,[(a,[(a,Int)])])] -> [[(a,[(a, Int)])]] 
purchasesIntersection _ []=[]
purchasesIntersection (list) (((user,items):t)) =helperpurchasesIntersection  list items : purchasesIntersection list t

helperpurchasesIntersection [] _=[]
helperpurchasesIntersection _ []=[]
helperpurchasesIntersection ((item,listofitems):t) ((item1,listofitems1):t1) = if(listofitems ==[] || listofitems1==[]) then helperpurchasesIntersection t t1 else  (item,(addPairForAllItems  listofitems listofitems1)) : helperpurchasesIntersection t t1

removeUser _ [] = []
removeUser u ((user, stats):t) = if u == user then t else (user, stats):removeUser u t
	
getStats user [] = []
getStats user ((u, stats):t) = if user == u then stats else getStats user t

freqListUsers :: String -> [(String, Int)]

freqListUsers user =helper3 (helper2 (purchasesIntersection userStat otherStats))
				where 
					allStats = getAllUsersStats purchasesHistory
					otherStats = removeUser user allStats
					userStat = getStats user allStats
					

helper2 [] =[]
helper2 (h:t)=h ++ (helper2 t)

helper3 [] =[]
helper3 ((item,items):t)= helper4 (items ++ helper3 t)
helper4 []=[]
helper4 (h:t)=addPair h (helper4 t)

recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user = helperreturnindex (helperrecommendEmptyCart2 (freqListUsers user))

recommend :: String -> [String] -> String
recommend user hiscart = if ((recommendBasedOnItemsInCart user hiscart ) : ((recommendBasedOnUsers user):[])==[])then(helperreturnindex(items)) else helperreturnindex(  (recommendBasedOnItemsInCart user hiscart ) : ((recommendBasedOnUsers user):[]) )
