Starting from the "English prose" formulation of the property we need to:

1. Rexpress it as first-order logical formula 
2. Encode the formula as an Imandra `theorem` or `verify` statement 
3. Make sure that Imandra proves/verifies the statement 

## Verification Goal 6
The VG6 is formulated as *"If the instrument is not in snapshot channel (illiquid instrument case), the book should be populated from incremental channel."* We want to express it as a statement about the `one_step` function: for any initial state that (a)  has  an illiquid sequirty (b) starts from a recovery state (c) has an incoming message. In that case, recovery is always always successful and the feed gets back to the normal state.  

We can formally write this requirement as a first-order logic formula:

![vg6](http://latex.codecogs.com/gif.latex?%5Cforall&space;s&space;%5Cleft%5B&space;%28Illiquid%28s%29%5Cland&space;InRecovery%28s%29&space;%5Cland&space;HasMessage%28s%29%29&space;%5Crightarrow&space;Normal%28one%5C_step%28s%29%29&space;%5Cright%5D)


    theorem illiquid_nat_refresh (s) =
     ( both_illiquid s &&
       s.feed_status = InRecovery &&
       get_next_message s <> None )
     ==>
     ((one_step s).feed_status = Normal)

## Verification Goal 5

![vg5](http://latex.codecogs.com/gif.latex?%5Cforall%20s%20%5Cforall%20m%20%5Cleft%5B%20one%5C_step%28setA%28s%2Cm%29%29%20%5Csimeq%20one%5C_step%28setB%28s%2Cm%29%29%20%5Cright%5D)

    verify one_step_A_B_invariant(s, m) = 
        let sA = set_next_message( s, m, Ch_Ref_A ) in
        let sB = set_next_message( s, m, Ch_Ref_B ) in
        states_eq(one_step sA, one_step sB)

## Verification Goal 4

![vg4](http://latex.codecogs.com/gif.latex?%5Cforall%20s%20%5Cleft%5B%20%28InRecovery%28s%29%20%5Cland%20NextMessageMakesCacheValid%28s%29%29%20%5Crightarrow%20Normal%28one%5C_step%28s%29%29%29%29%20%5Cright%5D)

    theorem recovery_both (s) =
        ( is_cache_valid_after_processing s && 
          s.feed_status = InRecovery 
        ) ==>
        (one_step s).feed_status = Normal
