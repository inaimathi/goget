var App = angular.module("goget", [])
    .config(function ($httpProvider) {
	/// Angular's post doesn't do the correct default thing with POST parameters
	$httpProvider.defaults.headers.post['Content-Type'] = 'application/x-www-form-urlencoded; charset=UTF-8';
	$httpProvider.defaults.transformRequest = function(data){
            return _.map(data, function (val, k) { return encodeURIComponent(k) + "=" + encodeURIComponent(val); }).join("&");
	}
    });

App.controller('GoGetCtrl', function ($scope, $http) {
    $scope.itemList = [];
    $scope.newItem = { count: 1 };
    $scope.user = { id: false, loggedIn: false, passphrase: "" };

    function itemPost (uri, params) {
	$http.post(uri, params)
	    .success(function (data) {
		$scope.itemList = data;
	    })
	    .error(function (data) {
		console.log(data);
	    })
    }

    function userPost (uri, params) {
	console.log("Sending " + uri + " request...")
	$http.post(uri, params)
	    .success(function (data) {
		$scope.user.id = data.id;
		$scope.user.loggedIn = true;
		$scope.itemList = data.items;
	    })
	    .error(function (data) {
		$scope.authError = data;
		console.log(data)
	    })
    }

    $scope.login = function (name, pass) {
	userPost("/auth/login", {name : name, passphrase: pass});
    }

    $scope.register = function (name, pass) {
	userPost("/auth/register", {name : name, passphrase: pass});
    }

    $scope.add = function (itemName, comment, count) {
	$http.post("/app/new", {itemName: itemName, comment: comment, count: count})
	    .success(function (data) {
		$scope.itemList = data;
		$scope.newItem = { count: 1 }
	    })
    }
    
    $scope.need = function (itemName) {
	itemPost("/app/item/need", {itemName: itemName});
    }
    
    $scope.got = function (itemName) {
	itemPost("/app/item/got", {itemName: itemName});
    }

});