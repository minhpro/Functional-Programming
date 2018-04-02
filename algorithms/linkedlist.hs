data ListNode a = ListNode { val :: a,
                             next :: ListNode a
                           } | Nil 
                                deriving Show

x :: ListNode Int
x = ListNode 10 (ListNode 20 Nil)

--remove from linked list
dropLinkedList :: Eq a => ListNode a -> a -> ListNode a
dropLinkedList Nil _ = Nil
dropLinkedList (ListNode val next) k 
    | k == val = dropLinkedList next k
    | otherwise = ListNode val (dropLinkedList next k)
    
--from anya_m from codefights
removeKFromList :: (Eq a) => ListNode a -> a -> ListNode a
removeKFromList Nil k = Nil
removeKFromList l k
    | (val l) == k = removeKFromList (next l) k
    | otherwise = ListNode (val l) (removeKFromList (next l) k) 