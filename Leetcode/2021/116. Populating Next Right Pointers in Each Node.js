
//----------------------------------------------------------------
// recursion
var connect = function(root) {
    if(!root) return root;
    connectTwoNodes(root.left,root.right);
    return root;    
};

function connectTwoNodes(left,right){
    if(!left||!right) return;

    left.next = right;

    connectTwoNodes(left.left,left.right);
    connectTwoNodes(right.left,right.right);
    connectTwoNodes(left.right,right.left);
}

//----------------------------------------------------------------