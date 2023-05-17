# Transfer

## Overview

*Transfers* are direct water connections from a reservoir or node to
another reservoir or node. Transfers are instantaneous movements of
water (and its constituents and particles) without any detailed
description of physics or storage. The Transfer View specifies the
connectivity of the transfer. A time series must also be listed in the
Transfer Time Series View to specify the flow -- the default is zero.

## Tables

**Example**

``` text
# Description:
# Sample transfer from a reservoir to a node

TRANSFER 
NAME       FROM_OBJ  FROM_IDENTIFIER TO_OBJ TO_IDENTIFIER 
transfer_1 reservoir res_1           node   6  
END
```

The Transfer table defines the name and connectivity of the transfer.
The flow is a time series input specified in
<a href="https://dwrnpmsweb0110/input_transfer_flow.html"
rel="nofollow">TRANSFER_TIME_SERIES</a>

#### Field Descriptions

##### NAME

Name of the transfer. This is the identifier of the transfer used in
other GUI views.

##### FROM_OBJ

Type (node or reservoir) of the source object.

##### FROM_IDENTIFIER

Identifier (node number or reservoir name) of the source destination
object.

##### TO_OBJ

Type (node or reservoir) of the destination object.

##### TO_IDENTIFIER

Identifier (node number or reservoir name) of the destination object.

  

  

In previous versions of DSM2, Transfers were called "obj2obj".

To complete the specification of a Transfer, a time series or constant
flow must be attached to it in the Transfer Time Series table.
