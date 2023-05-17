# Merging multiple versions of network files

One way to merge changes from multiple users is to use a file comparison
tool such as WinMerge. However, if the users did not start with
identical versions of the network file, this won't work.

I have added a feature to the CSDP (Network-Save Specified Channels),
which helps merge changes from network files submitted by multiple users
who did not start with the same version of the network file.

Here's how I use it to merge changes into an existing network file:

1.  Get a list of modified centerlines for the new network file.
2.  Enter the list into Excel. 
3.  Copy the list, and paste-special-transpose.
4.  In the CSDP, load the existing network file, which you might refer
    to as the current master version. 
    1.  Select Network-Save Specified Channels.
    2.  In the file selector dialog, enter a filename for the new master
        version. 
    3.  Go back to Excel, and copy the transposed list of centerline
        numbers. Past them into the dialog that appears (below) in the
        Channel Numbers field. (The list will be tab delimited, which is
        fine). You may not be able to see all the centerline names in
        the text field (I'll have to work on that), but it will work.
    4.  Before clicking OK, click the "Don't export specified channels"
        checkbox. **Make sure this option is selected before you click
        OK**. When you click OK, CSDP will create a new network file
        containing all centerlines EXCEPT the ones you specified.  
        <img src="attachments/87228850/87228851.png"
        data-image-src="attachments/87228850/87228851.png"
        data-unresolved-comment-count="0" data-linked-resource-id="87228851"
        data-linked-resource-version="1" data-linked-resource-type="attachment"
        data-linked-resource-default-alias="image2018-12-21_12-0-36.png"
        data-base-url="http://msb-confluence"
        data-linked-resource-content-type="image/png"
        data-linked-resource-container-id="87228850"
        data-linked-resource-container-version="1" height="250" />
5.  Now load the new network file.
    1.  Select Network-Save Specified Channels.
    2.  In the file selector dialog, enter a filename for a temporary
        network file.
    3.  Go back to Excel, and copy the transposed list of centerline
        numbers. Paste them into the dialog that appears (below) in the
        Channel Numbers field. (The list will be tab delimited, which is
        fine). You may not be able to see all the centerline names in
        the text field (I'll have to work on that), but it will
        work.<img src="attachments/87228850/87228849.png"
        data-image-src="attachments/87228850/87228849.png"
        data-unresolved-comment-count="0" data-linked-resource-id="87228849"
        data-linked-resource-version="1" data-linked-resource-type="attachment"
        data-linked-resource-default-alias="image2018-12-21_12-8-47.png"
        data-base-url="http://msb-confluence"
        data-linked-resource-content-type="image/png"
        data-linked-resource-container-id="87228850"
        data-linked-resource-container-version="1" height="250" />
    4.  Before clicking OK, **make sure the "Don't export specified
        channels" checkbox is NOT selected**. When you click OK, CSDP
        will create a new network file containing only the centerlines
        you specified. 
6.  Use a text editor to copy the contents (excluding the headers at the
    top) of the new temporary network file into the new master network
    file. Update the numElements field in the header of the new file.
    The value should be the sum of the values from the two files.

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2018-12-21_12-8-47.png](attachments/87228850/87228849.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2018-12-21_12-0-36.png](attachments/87228850/87228851.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2018-12-21_11-49-11.png](attachments/87228850/87228852.png)
(image/png)  
