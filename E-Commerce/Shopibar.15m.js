#!/usr/bin/env /usr/local/bin/node

/*
 * <bitbar.title>Shopibar</bitbar.title>
 * <bitbar.version>v1.0.1</bitbar.version>
 * <bitbar.author>Etka Özdemir</bitbar.author>
 * <bitbar.author.github>m-etka</bitbar.author.github>
 * <bitbar.image>http://i.imgur.com/Dt499Nh.png</bitbar.image>
 * <bitbar.desc>Shopify Admin Plugin for BitBar</bitbar.desc>
 * <bitbar.dependencies>node</bitbar.dependencies>
 *
 */

/*
 * Visit http://shopibar.etka.org for installation and details
 */


/* Shopibar configuration */
var admin = {
    myShopifyAccountName: 'example.myshopify.com',
    apiKey: 'example.com_api_key',
    password: 'example.com_password'
};
showCustomerCount = true;
showPendingOrders = true;
showUnshippedOrders = true;
/* End of Shopibar configuration  */

var verbose = true;

// Start plug-in
console.log('🛒');
console.log('---');
printBitBarLine('Shopibar', ['color=black', 'font=Phosphate', 'size=20']);


// Get data using Shopify Admin API
get(admin, 'shop', function(data) {
    // Show shop info
    printBitBarLine(data.shop.name, ['color=black', 'font=Calibri', 'size=18']);

    if (showCustomerCount) {
        // Get customer count
        get(admin, 'customerCount', function(data) {
            // Show customer count
            printBitBarLine('Customer Count:', ['color=gray', 'font=Calibri', 'size=14']);
            printBitBarLine(data.count, ['color=black', 'font=Calibri', 'size=16']);
        });
    }

    if (showPendingOrders) {
        // Get pending orders
        get(admin, 'pendingOrders', function(data) {
            printOrders(data, 'Pending Orders');
        });
    }

    if (showUnshippedOrders) {
        // Get unshipped orders
        get(admin, 'unshippedOrders', function(data) {
            printOrders(data, 'Unshipped Orders');
        });
    }

});

// Print order details
function printOrders(data, title) {
    // Show orders
    printBitBarLine(' ');
    printBitBarLine(title + ':', ['color=gray', 'font=Calibri', 'size=14']);
    printBitBarLine(data.orders.length, ['color=black', 'font=Calibri', 'size=16']);
    for (var i = 0; i < data.orders.length; i++) {
        // Show order price and user info
        printBitBarLine('--$' + data.orders[i].total_line_items_price, ['color=black', 'font=Calibri', 'size=16']);
        printBitBarLine('--' + data.orders[i].email, ['font=Calibri', 'size=14']);
        // Show items ordered
        for (var j = 0; j < data.orders[i].line_items.length; j++) {
            if (j !== 0) {
                printBitBarLine('----');
            }
            printBitBarLine('----' + data.orders[i].line_items[j].title, ['font=Calibri', 'size=16', 'color=black']);
            printBitBarLine('----');
            printBitBarLine('----Name     : ' + data.orders[i].line_items[j].name, ['font=Monaco', 'size=11']);
            printBitBarLine('----SKU      : ' + data.orders[i].line_items[j].sku, ['font=Monaco', 'size=11']);
            printBitBarLine('----Price    : $' + data.orders[i].line_items[j].price, ['font=Monaco', 'size=11']);
            printBitBarLine('----Quantity : ' + data.orders[i].line_items[j].quantity, ['font=Monaco', 'size=11']);

        }
        printBitBarLine('-----');

    }
}


// Get
function get(admin, requestName, callback) {
    var endpoint = {
        url: '',
        requestType: 'GET'
    };
    switch (requestName) {
        case 'shop':
            endpoint.url = '/admin/shop.json';
            endpoint.requestType = 'GET';
            break;
        case 'customerCount':
            endpoint.url = '/admin/customers/count.json';
            endpoint.requestType = 'GET';
            break;
        case 'pendingOrders':
            endpoint.url = '/admin/orders.json?status=pending';
            endpoint.requestType = 'GET';
            break;
        case 'unshippedOrders':
            endpoint.url = '/admin/orders.json?fulfillment_status=unshipped';
            endpoint.requestType = 'GET';
            break;
        default:
            break;
    }
    performRequest(admin, endpoint.url, endpoint.requestType, function(data) {
        if (data.errors && verbose) {
            console.log('Errors: ' + data.errors);
        } else {
            callback(data);
        }
    });

}

// HTTPS request 
function performRequest(admin, endpoint, method, success) {
    var https = require('https');
    var auth = new Buffer(admin.apiKey + ':' + admin.password).toString('base64');

    var headers = {};

    headers = {
        'Authorization': 'Basic ' + auth,
        'Content-Type': 'application/json'
    };

    var options = {
        host: admin.myShopifyAccountName,
        path: endpoint,
        method: method,
        headers: headers
    };

    var req = https.request(options, function(res) {
        res.setEncoding('utf-8');

        var responseString = '';

        res.on('data', function(data) {
            responseString += data;
        });

        res.on('end', function() {
            var responseObject = JSON.parse(responseString);
            success(responseObject);
        });
    });

    req.end();

    req.on('error', function(err) {
        console.log('Error: Could not connect! Check your credientials and connection.');
        if (verbose) {
            console.log(err);
        }
    });
}

// Plug-in output
function printBitBarLine(title, args) {
    var lineArgs = [];

    if (title !== '-----') {
        for (var i in args) {
            lineArgs.push(args[i]);
        }

        if (lineArgs.length !== 0) {
            title = title + '|';
        }
    }
    console.log(title + lineArgs.join(' '));
}