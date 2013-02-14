var util = {
    hcompile: function (template) {
	return Handlebars.compile($("#tmp-" + template).html())
    },
    vals: function (listOfDOMSelectors) {
	return _.map(listOfDOMSelectors, function (s) { return $(s).val() })
    },
    under: function (DOMContext, listOfDOMSelectors) {
	return _.map(listOfDOMSelectors, function (s) { return DOMContext + s })
    },
    applyToVals: function (fn, DOMContext, listOfDOMSelectors) {
	return fn.apply({}, util.vals(util.under(DOMContext, listOfDOMSelectors)));
    },
    applyToUser: function (fn) {
	return util.applyToVals(fn, '.user-form ', ['.user-name', '.passphrase']);
    }
}

Handlebars.registerHelper("controls", function (anItem) {
    if (anItem.status == 'Got') {
	var ctrl = {fn: 'need', iconClass: "icon-exclamation-sign"}
    } else {
	var ctrl = {fn: 'got', iconClass: "icon-check"}
    }
    return new Handlebars.SafeString(templates.itemButtons(ctrl));
})

var templates = {
    item: util.hcompile("item"),
    itemButtons: util.hcompile("item-controls")
}

var goget = {
    render: function (itemList) {
	$(".shopping-list-controls").show()
	$(".shopping-list").empty();
	$.each(itemList, function (ix, anItem) {
	    $(".shopping-list").append(templates.item(anItem));
	})
    },
    itemPost: function (uri, params) {
	$.post(uri, params)
	    .done(function (data, textStatus, jqXHR) {
		goget.render($.parseJSON(jqXHR.responseText))
	    })
	    .fail(function (data, textStatus, jqXHR) {
		console.log(["Failed!", data, textStatus, jqXHR])
		// something odd happened; either invalid item, or failed connection
	    })
    },
    userPost: function (uri, params) {
	$.post(uri, params)
	    .done(function (data, textStatus, jqXHR) {
		$(".user-form").hide();
		goget.render($.parseJSON(jqXHR.responseText).items);
	    })
	    .fail(function (data) {
		console.log(["Failed!", data.responseText])
		$(".user-form .error").text(data.responseText).show()
	    })
    },
    login: function (name, pass) {
	goget.userPost("/auth/login", { name: name, passphrase: pass });
    },
    register: function (name, pass) {
	goget.userPost("/auth/register", { name: name, passphrase: pass });
    },
    add: function (itemName, comment, count) {
	goget.itemPost("/app/new", {itemName: itemName, comment: comment, count: count})
    },
    need: function (itemName) {
	goget.itemPost("/app/item/need", {itemName: itemName});
    },
    got: function (itemName) {
	goget.itemPost("/app/item/got", {itemName: itemName});
    }
    
}