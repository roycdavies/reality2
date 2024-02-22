# ------------------------------------------------------------------------------------------------------------------------------------------------------
# A Finite State Machine or Automation
# ------------------------------------------------------------------------------------------------------------------------------------------------------
class FSM:
	var _current_state: String
	var _transitions = []
	var _event_queue = []
	var _timed_events = []
	var _debug = false
	func set_debug(val): _debug = val
	
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Constructor
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _init():
		_current_state = "start"
		queue_event("init", {})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Public functions
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func add_transition(from, event, to, actions = []):
		var transition = {"from": from, "event": event, "to": to, "actions": actions}
		if (_debug): print("Adding Transition: ", transition)
		_transitions.push_back(transition)

	func queue_event(event: String, parameters = {}, delay = 0):
		if (delay == 0):
			_event_queue.push_back({"event": event, "parameters": parameters})
		else:
			_timed_events.push_back({"event": event, "parameters": parameters, "time": Time.get_ticks_msec() + delay * 1000})
	
	# Should be called every frame (eg in the _process function of a node)
	func step():
		# Check the timed events and queue any that have expired
		for i in range(_timed_events.size() - 1, -1, -1):
			if (_timed_events[i].time <= Time.get_ticks_msec()):
				queue_event(_timed_events[i].event, _timed_events[i].parameters)
				_timed_events.remove_at(i)
		
		# Check the events and perform transitions
		if _event_queue.size() > 0:
			var event_and_parameters = _event_queue.pop_front()
			_check_transitions(event_and_parameters)
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Private functions
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _check_transitions(event_and_parameters):
		for transition in _transitions:
			if (_current_state == transition.from || transition.from == "*"):
				if (event_and_parameters.event == transition.event):
					if (_debug): print(transition.from, ":", transition.event, "->", transition.to)
					if(transition.to != "*"):
						_current_state = transition.to
					var previous_result = event_and_parameters.parameters
					for action in transition.actions:
						previous_result = action.call(previous_result)
					break
	# --------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
