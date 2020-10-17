'''
This code was written by Florianne Verkroost. This code implements a simplified version 
of the agent-based model described in Kalick, S. M., & Hamilton III, T. E. (1986) 
"The matching hypothesis reexamined". Journal of Personality and Social Psychology, 
51(4):673-682. This code is NOT intended to replicate the paper or make any inferences 
(neither on its own nor in any relation to the paper) whatsoever, but merely uses the 
model described in the paper as a "case study" for the implementation of an agent-based 
model using Mesa in Python.
'''


'''
Import libraries and packages
'''
from mesa import Model, Agent
from mesa.time import RandomActivation
from mesa.space import SingleGrid
from mesa.datacollection import DataCollector
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

'''
Agent (human) class with attributes and a step function
defining what actions to perform during each step
'''
class Human(Agent):

    def __init__(self, unique_id, model, sex, attract, relationship_status, current_dating_partners, focal):
        '''
         Create a new agent.
         Args:
            sex: agent's sex (female or male)
            attract: agent's attractiveness (between 1 and 10)
            relationship_status: agent's marital status (single or union)
            current_dating_partners: list of id's of agent's current dating current_dating_partners
            focal: agent's probability to start a union
        '''
        super().__init__(unique_id, model)
        self.sex = sex
        self.attract = attract
        self.relationship_status = relationship_status
        self.current_dating_partners = current_dating_partners
        self.focal = focal

    def step(self):
        '''
        Have agent take a step: if single, select a dating partner (if available),
        compute daters' probabilities of starting a union and update their
        relationship status if they start a union.
        '''

        # Only perform actions for those who are still single
        if self.relationship_status == "single":

            # Select a dating partner (needs to be from the other sex, single and not dating anyone else)
            other_sex = [sex for sex in ["male", "female"] if sex != self.sex][0]
            available_daters = [agent for agent in self.model.schedule.agents if agent.sex == other_sex and agent.relationship_status == "single" and len(agent.current_dating_partners) == 0]
            if len(available_daters) > 0:
                selected_dater = available_daters[np.random.randint(0, len(available_daters))]
                selected_dater.current_dating_partners = selected_dater.current_dating_partners + [self.unique_id]
                self.current_dating_partners = self.current_dating_partners + [selected_dater.unique_id]

            # Calculate the probabilities of starting a union given the type of preference
            if len(self.current_dating_partners) > 0:
                id_current_date = self.current_dating_partners[-1]
                current_date = [agent for agent in self.model.schedule.agents if agent.unique_id == id_current_date][0]
                P1 = ((current_date.attract**3) / 1000)
                P1C = (P1 ** ((51 - self.model.schedule.steps) / 50))
                P2 = (((10 - abs(self.attract - current_date.attract))**3) / 1000)
                P2C = (P2 ** ((51 - self.model.schedule.steps) / 50))
                P3 = ((P1 + P2) / 2)
                P3C = (P3 ** ((51 - self.model.schedule.steps) / 50))
                if self.model.preference == "attractiveness":
                    self.focal = P1C
                elif self.model.preference == "matching":
                    self.focal = P2C
                else:
                    self.focal = P3C

                # Check if two agents want to start a union and update their relationship status accordingly
                # Also update the table union_table that keeps track of the attractiveness of couples
                if np.random.uniform(0, 1) < self.focal and np.random.uniform(0, 1) < current_date.focal:
                    self.relationship_status = "union"
                    current_date.relationship_status = "union"
                    if self.sex == "male":
                        data = pd.DataFrame({"male_attract": [self.attract], "female_attract": [current_date.attract]})
                    else:
                        data = pd.DataFrame({"male_attract": [current_date.attract], "female_attract": [self.attract]})
                    self.model.union_table = self.model.union_table.append(data)

'''
Model class for the matching model.
'''
class KalickHamilton(Model):

    def __init__(self, number_agents_per_sex = 100, preference = "attractiveness", union_table = pd.DataFrame(), final_result = pd.DataFrame(), attract_model = "uniform", attract_mu = 6, attract_sigma = 1):
        '''
        Create the model.
         Args:
            number_agents_per_sex: number of agents per sex (total number of agents is twice the number of agents per sex)
            preference: matching preference (should be either matching, mixed or attractiveness)
            union_table: data frame that keeps track of couples' attractiveness (one row per couple)
            final_result: data frame with for each step the percentage of unions, the correlation between partners' attractiveness and the average attractiveness of all those in union
            attract_model: should attractiveness be drawn from Uniform or Normal distribution?
            attract_mu, attract_sigma: mean and standard deviation from Normal distribution
            '''
        self.number_agents_per_sex = number_agents_per_sex
        self.preference = preference
        self.union_table = union_table
        self.final_result = final_result
        self.attract_model = attract_model
        self.attract_mu = attract_mu
        self.attract_sigma = attract_sigma

        # Start the scheduler (random activation)
        self.schedule = RandomActivation(self)

        # Add agents to the model scheduler
        for i in range(number_agents_per_sex):
            if self.attract_model == "uniform":
                att_value_male = np.random.randint(1, 11)
                att_value_female = np.random.randint(1, 11)
            elif self.attract_model == "normal":
                att_value_male = np.random.normal(self.attract_mu, self.attract_sigma, 1)[0]
                att_value_female = np.random.normal(self.attract_mu, self.attract_sigma, 1)[0]
            agent_male = Human(unique_id = (2*(i+1)-1), model = self, sex = "male", attract = att_value_male, relationship_status = "single", current_dating_partners = [], focal = 0)
            agent_female = Human(unique_id = (2*(i+1)), model = self, sex = "female", attract = att_value_female, relationship_status = "single", current_dating_partners = [], focal = 0)
            self.schedule.add(agent_male)
            self.schedule.add(agent_female)

        # Run the model
        self.running = True

    def step(self):
        '''
        Run one step of the model. Simulation model should stop (stop) either when all
        agents are in a union, or when 51 ticks have been reached.
        '''

        # Perform one step and print the step number
        print("Step: " + str(self.schedule.steps))
        self.schedule.step()

        # Remove links between single individuals
        all_agents = self.schedule.agents
        for agent in all_agents:
            currently_dating = agent.current_dating_partners
            agent.current_dating_partners = [agent.unique_id for agent in all_agents if agent.unique_id in currently_dating and agent.relationship_status == "single"]

        # If there are at least two unions, compute the mean attractiveness
        # of those in union and the correlation between male and female attractiveness
        if self.union_table.shape[0] >= 2:
            correl = self.union_table.corr().iloc[0, 1]
            attract_values = list(self.union_table["male_attract"])  + list(self.union_table["female_attract"])
            mean_attract = np.mean(attract_values)
            percentage_union = self.union_table.shape[0] / self.number_agents_per_sex
            data = pd.DataFrame({"mean_attract": [mean_attract], "correlation": [correl], "perc_union": [percentage_union]})
            self.final_result = self.final_result.append(data)
            self.final_result.to_csv('final_result.csv')

    # Run the model under aforementioned conditions
    def run_model(self):
        while self.union_table.shape[0] < self.number_agents_per_sex and self.schedule.steps <= 50:
            self.step()

# Run the model
mod = KalickHamilton()
mod.run_model()

# Plot the results
df = pd.read_csv('final_result.csv')
df.sort_values('perc_union')
plt.subplot(1, 2, 1)
plt.plot(df.perc_union * 100, df.correlation)
plt.xlabel('Percentage in union')
plt.ylabel("Correlation between partners' attractiveness")
plt.subplot(1, 2, 2)
plt.plot(df.perc_union * 100, df.mean_attract)
plt.xlabel('Percentage in union')
plt.ylabel("Average attractiveness of all those in union")
plt.show()
