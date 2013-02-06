var App = angular.module("goget", [])
    .config(function ($httpProvider) {
	/// Angular's post doesn't do the correct default thing with POST parameters
	$httpProvider.defaults.headers.post['Content-Type'] = 'application/x-www-form-urlencoded; charset=UTF-8';
	$httpProvider.defaults.transformRequest = function(data){
            return _.map(data, function (val, k) { return encodeURI(k) + "=" + encodeURI(val); }).join("&");
	}
    });

App.controller('GoGetCtrl', function ($scope, $http) {
    $scope.itemList = [];
    $scope.newItem = { count: 1 };
    $scope.user = {};
    $scope.loggedIn = false;

    $scope.itemPost = function (uri, params) {
	$http.post(uri, params)
	    .success(function (data) {
		$scope.itemList = data;
	    })
    }

    $scope.login = function (name, pass) {
	console.log("Sending login request...");
	$scope.loggedIn = true;
	$http.post("/login")
	    .success(function (data) {
		console.log("Got login response...")
		console.log("Sending list request...")
		$http.get("/list").success(function (data) {
		    console.log("Got list response...")
		    $scope.itemList = data;
		})
	    })
    }

    $scope.add = function (itemName, comment, count) {
	$http.post("/new", {itemName: itemName, comment: comment, count: count})
	    .success(function (data) {
		$scope.itemList = data;
		$scope.newItem = { count: 1 }
	    })
    }
    
    $scope.comment = function (item, comment) {
	$scope.itemPost("/")
    }

    $scope.need = function (itemName) {
	$scope.itemPost("/need", {itemName: itemName});
    }
    
    $scope.got = function (itemName) {
	$scope.itemPost("/got", {itemName: itemName});
    }

    $scope.login("Test", "password");

});