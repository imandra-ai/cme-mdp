![Aesthetic Integration](https://storage.googleapis.com/imandra-assets/images/docs/iml_cme_mdp_model.svg)

## CME Exchange model 
The `CME_Exchange` is a model used to generate sequences of packets for the CME
model. The model takes in a sequence of actions and, if the sequence is_valid,
generates a sequence of outgoing packets. Every action can either result in
creation of a security-related message, stored in the model state, or it can
result in the list of the stored messages being sent as a packet. The model
stores the sequence numbers of the messages ( each security has its own) and
sequence numbers of the packets.

In more details, the state of the exchange model contains:
- **`sec_a`** and **`sec_b`** : security states for two simulated securities
- **`inc_msg_queue`** : list of Incremental Refresh messages that are to be sent in the next refresh packet
- **`snap_msg_queue`**  – list of Snapshot messages that are to be sent in the next snapshot packet
- **`last_inc_seq_num`** – last packet sequence number for refresh channels
- **`last_snap_seq_num`** – last packet sequence number for snapshot channels
- **`pac_queue`** – list of generated packets
The security state structure contains:
- **`last_rep_seq`_num`** -  this security RptSeqNumber incremented with each message
- **`sec_id`** – numerical ID of the security 
- **`multi_book`** and **`implied_book`**  – the state of the two books 
For the books we are storing five levels for buy and sell sides:
```ocaml
type book_side = {
    one : order_level;
    two : order_level;
    three : order_level;
    four : order_level;
    five : order_level;
};;
type order_book = {
    buy_orders : book_side;
    sell_orders : book_side;
};;
```
