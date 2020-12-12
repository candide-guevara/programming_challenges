import qualified Text.Printf as T

data FSItem = File String String | Folder String [FSItem] deriving Show
data Crumb  = Crumb String [FSItem] [FSItem] deriving Show
data Path   = Path FSItem [Crumb]

go_up :: Path -> Path
go_up (Path f (c:cs)) = let folder = rebuild_folder f c
                       in Path folder cs
go_up root          = root

go_down :: Path -> Path
go_down (Path d cs) = case d of
                        Folder name files -> Path (head files) $ (Crumb name (tail files) []):cs
                        otherwise         -> undefined

pick_by_name :: String -> Path -> Path
pick_by_name name (Path f (c:cs)) = let 
                                        (Crumb dirname z0 z1) = c
                                        file_list = foldl (\a x -> x:a) (f:z0) z1
                                        (new_f, z0' , z1') = find_item_by_name name file_list
                                     in Path new_f $ (Crumb dirname z0' z1'):cs
pick_by_name _ _                  = undefined

rebuild_folder :: FSItem -> Crumb -> FSItem
rebuild_folder f (Crumb name z0 z1) = let file_list = foldl (\a x -> x:a) (f:z0) z1
                                       in Folder name file_list

find_item_by_name :: String -> [FSItem] -> (FSItem, [FSItem], [FSItem])
find_item_by_name name files = let f = head files
                                   z1 = tail files
                                   item_name = case f of
                                     File name _   -> name
                                     Folder name _ -> name
                                in case item_name == name of
                                  True  -> (f, z1, [])
                                  False -> let (f', z0', z1') = find_item_by_name name z1
                                            in (f', z0', z1' ++ [f])

my_disk :: FSItem  
my_disk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_bananas.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]  
ini_path = Path my_disk []
ape_path = ((pick_by_name "ape_throwing_bananas.jpg") . go_down . (pick_by_name "pics") . go_down) ini_path
Path ape_file _ = ape_path

main = do 
  T.printf "file = %s\n" (show ape_file)
  T.printf "done\n"

