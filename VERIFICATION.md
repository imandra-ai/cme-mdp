Starting from the "English prose" formulation of the property we need to:

1. Rexpress it as first-order logical formula 
2. Encode the formula as an Imandra `theorem` or `verify` statement 
3. Make sure that Imandra proves/verifies the statement 

##Verification Goal 6
The VG6 is formulated as *"If the instrument is not in snapshot channel
(illiquid instr case), the book should be populated from incremental channel."*
We want to express it as a statement about the `one_step` function: for any
initial state that (a)  has  an illiquid sequirty (b) starts from a recovery
state (c) has an incoming message. In that case, recovery is always always
successful and the feed gets back to the normal state.  

We can formally write this requirement as a first-order logic formula:

![vg6](http://latex.codecogs.com/gif.latex?%5Cforall&space;s&space;%5Cleft%5B&space;%28Illiquid%28s%29%5Cland&space;InRecovery%28s%29&space;%5Cland&space;HasMessage%28s%29%29&space;%5Crightarrow&space;Normal%28one%5C_step%28s%29%29&space;%5Cright%5D)


    theorem illiquid_nat_refresh (s) =
     ( both_illiquid s &&
       s.feed_status = InRecovery &&
       get_next_message s <> None )
     ==>
     ((one_step s).feed_status = Normal)



##Verification Goal 5

The MDP messages are gathered into packets and sent over a UDP connection. In
order to reduce the chance of a packet loss, two redundant channels (called A
and B) are used. Our Imandra model also simulates this behavior.

For the VG5 we want to formally verify that *"The book must be up-to-date no
matter if A or B channel is behind or completely empty."*  We are going to
prove an even more general statement -- for any feed state `s` and any incoming
message `m`, processing the message coming from channel A and processing the
message coming from channel B will get you a pair of equivalent states:

![vg5](http://latex.codecogs.com/gif.latex?%5Cforall%20s%20%5Cforall%20m%20%5Cleft%5B%20one%5C_step%28setA%28s%2Cm%29%29%20%5Csimeq%20one%5C_step%28setB%28s%2Cm%29%29%20%5Cright%5D)

Where the equivalence operation "~=" checks that the relevant parts of the
state are being equal: in our case these are the feed status, the last message
number processed and everything in the books state. We encode this equivalence
operator as a `states_eq` function. 
    let states_eq(s1, s2) =
        ( s1.feed_status = s2.feed_status ) &&
        ( s1.channels.last_seq_processed = s2.channels.last_seq_processed ) &&
        ( s1.books = s2.books ) 

And we encode our verification goal using the `states_eq` function and the
`set_next_message(s,m,c)` function, which creates a packet in channel `c` with
a single message `m` and places the packeet in the list of incoming packets. 

    verify one_step_A_B_invariant(s, m) = 
        let sA = set_next_message( s, m, Ch_Ref_A ) in
        let sB = set_next_message( s, m, Ch_Ref_B ) in
        states_eq(one_step sA, one_step sB)

As with the VG6, Imandra cannot prove our goal statement right away. We
simplify the task for Imandra, by proving (as rewrite rules) the equivalence
for `process_msg_normal` and `process_msg_recovery` branches of the model and
then `:disable`ing both functions.    

##Verification Goal 4

Due to unreliability of UDP, the MDP receiver must have a strategy for dealing
with lost and out-of-sequence messages. The feed transitions to a recovery
state if the next message sequence number is wrong. In the recovery state, the
feed maintains the cache of all the incoming refresh messages -- if at certain
point the cache contains all the necessary messages to recover (valid cache),
then all the messages in cache are replayed and the feed transitions back to
the normal state.

This behavior is expressed as a VG4: *"If the packet is missed, the book must
be empty until recovered (via snapshot or natural refresh)"* 



![vg4](http://latex.codecogs.com/gif.latex?%5Cforall%20s%20%5Cleft%5B%20%28InRecovery%28s%29%20%5Cland%20NextMessageMakesCacheValid%28s%29%29%20%5Crightarrow%20Normal%28one%5C_step%28s%29%29%29%29%20%5Cright%5D)

    theorem recovery_both (s) =
        ( is_cache_valid_after_processing s && 
          s.feed_status = InRecovery 
        ) ==>
        (one_step s).feed_status = Normal
