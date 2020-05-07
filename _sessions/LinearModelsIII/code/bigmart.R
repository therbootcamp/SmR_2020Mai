
bigmart = read_csv('_sessions/LinearModelsIII/1_Data/bigmart_en.csv') 

bigmart = bigmart %>% 
  select(Item_Outlet_Sales, Item_Weight, Item_Fat_Content, Item_Visibility, Item_Type, Item_MRP, Outlet_Type) %>% 
  rename(Verkäufe = Item_Outlet_Sales, 
         Gewicht = Item_Weight, 
         Fettgehalt = Item_Fat_Content,
         Visibilität = Item_Visibility, 
         Typ = Item_Type, 
         Max_Preis = Item_MRP,
         Ladengrösse = Outlet_Type)

write_csv(bigmart, '_sessions/LinearModelsIII/1_Data/bigmart.csv')

cat(paste0('|',names(bigmart),'||'),sep='\n')



